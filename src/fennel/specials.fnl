(local utils (require "fennel.utils"))
(local parser (require "fennel.parser"))
(local compiler (require "fennel.compiler"))
(local unpack (or _G.unpack table.unpack))

(local SPECIALS compiler.scopes.global.specials)

(fn wrap-env [env]
  "Convert a fennel environment table to a Lua environment table.
This means automatically unmangling globals when getting a value,
and mangling values when setting a value. This means the original env
will see its values updated as expected, regardless of mangling rules."
  (setmetatable
   [] {:__index (fn [_ key]
                  (if (= (type key) "string")
                      (. env (compiler.global-unmangling key))
                      (. env key)))
       :__newindex (fn [_ key value]
                     (if (= (type key) "string")
                         (tset env (compiler.global-mangling key) value)
                         (tset env key value)))
       ;; checking the __pairs metamethod won't work automatically in Lua 5.1
       ;; sadly, but it's important for 5.2+ and can be done manually in 5.1
       :__pairs (fn []
                  (fn putenv [k v]
                    (values (if (= (type k) "string")
                                (compiler.global-unmangling k) k) v))
                  (values next (utils.kvmap env putenv) nil))}))

(fn current-global-names [env]
  (utils.kvmap (or env _G) compiler.global-unmangling))

(fn load-code [code environment filename]
  "Load code with an environment in all recent Lua versions"
  (let [environment (or (or environment _ENV) _G)]

    (if (and _G.setfenv _G.loadstring)
        (let [f (assert (_G.loadstring code filename))]
          (_G.setfenv f environment)
          f)
        (assert (load code filename "t" environment)))))

(fn doc* [tgt name]
  "Return a docstring for tgt."
  (if (not tgt)
      (.. name " not found")
      (let [docstring (: (: (or (: compiler.metadata "get" tgt "fnl/docstring")
                                "#<undocumented>") :gsub "\n$" "")
                         :gsub "\n" "\n  ")]
        (if (= (type tgt) "function")
            (let [arglist (table.concat (or (: compiler.metadata "get"
                                               tgt "fnl/arglist")
                                            ["#<unknown-arguments>"]) " ")]
              (string.format "(%s%s%s)\n  %s" name
                             (if (> (# arglist) 0) " " "") arglist docstring))
            (string.format "%s\n  %s" name docstring)))))

;; TODO: replace this with using the special fn's own docstring
(fn doc-special [name arglist docstring]
  "Add a docstring to a special form."
  (tset compiler.metadata (. SPECIALS name) {:fnl/arglist arglist
                                             :fnl/docstring docstring}))

(fn compile-do [ast scope parent start]
  "Compile a list of forms for side effects."
  (let [start (or start 2)
        len (# ast)
        sub-scope (compiler.make-scope scope)]
    (for [i start len]
      (compiler.compile1 (. ast i) sub-scope parent {:nval 0}))))

(fn SPECIALS.do [ast scope parent opts start chunk sub-scope pre-syms]
  "Implements a do statement, starting at the 'start'-th element.
By default, start is 2."
  (let [start (or start 2)
        sub-scope (or sub-scope (compiler.make-scope scope))
        chunk (or chunk [])
        len (# ast)]
    (var outer-target opts.target)
    (var outer-tail opts.tail)
    (var retexprs {:returned true})

    ;; See if we need special handling to get the return values of the do block
    (if (and (not outer-target)
             (not= opts.nval 0)
             (not outer-tail))
        (if opts.nval
            ;; generate a local target
            (let [syms []]
              (for [i 1 opts.nval 1]
                (local s (or (and pre-syms (. pre-syms i)) (compiler.gensym scope)))
                (tset syms i s)
                (tset retexprs i (utils.expr s "sym")))
              (set outer-target (table.concat syms ", "))
              (compiler.emit parent (: "local %s" :format outer-target) ast)
              (compiler.emit parent "do" ast))
            ;; we will use an IIFE for the do
            (let [fname (compiler.gensym scope)
                  fargs (or (and scope.vararg "...") "")]
              (compiler.emit parent (: "local function %s(%s)" :format
                                       fname fargs) ast)
              (set retexprs (utils.expr (.. fname "(" fargs ")") "statement"))
              (set outer-tail true)
              (set outer-target nil)))
        (compiler.emit parent "do" ast))
    ;; Compile the body
    (if (< len start)
        ;; In the unlikely event we do a do with no arguments
        (compiler.compile1 nil sub-scope chunk {:tail outer-tail
                                               :target outer-target})
        ;; There will be side-effects
        (for [i start len]
          (let [subopts {:nval (or (and (not= i len) 0) opts.nval)
                         :tail (or (and (= i len) outer-tail) nil)
                         :target (or (and (= i len) outer-target) nil)}]
            (utils.propagate-options opts subopts)
            (local subexprs (compiler.compile1 (. ast i) sub-scope chunk subopts))
            (when (not= i len)
              (compiler.keep-side-effects subexprs parent nil (. ast i))))))
    (compiler.emit parent chunk ast)
    (compiler.emit parent "end" ast)
    retexprs))

(doc-special "do" ["..."] "Evaluate multiple forms; return last value.")

(fn SPECIALS.values [ast scope parent]
  "Unlike most expressions and specials, 'values' resolves with multiple
values, one for each argument, allowing multiple return values. The last
expression can return multiple arguments as well, allowing for more than
the number of expected arguments."
  (let [len (# ast)
        exprs []]
    (for [i 2 len]
      (let [subexprs (compiler.compile1 (. ast i) scope parent
                                        {:nval (and (not= i len) 1)})]
        (tset exprs (+ (# exprs) 1) (. subexprs 1))
        (when (= i len)
          (for [j 2 (# subexprs) 1]
            (tset exprs (+ (# exprs) 1) (. subexprs j))))))
    exprs))

(doc-special "values" ["..."]
            "Return multiple values from a function. Must be in tail position.")

(fn SPECIALS.fn [ast scope parent]
  (var (index fn-name is-local-fn docstring) (values 2 (utils.is-sym (. ast 2))))
  (let [f-scope (doto (compiler.make-scope scope)
                 (tset :vararg false))
        f-chunk []
        multi (and fn-name (utils.is-multi-sym (. fn-name 1)))]
    (compiler.assert (or (not multi) (not multi.multi-sym-method-call))
                     (.. "unexpected multi symbol " (tostring fn-name))
                     (. ast index))

    (if (and fn-name (not= (. fn-name 1) "nil"))
        (do (set is-local-fn (not multi))
            (if is-local-fn
                (set fn-name (compiler.declare-local fn-name [] scope ast))
                (set fn-name (. (compiler.symbol-to-expression fn-name scope) 1)))
            (set index (+ index 1)))
        (do (set is-local-fn true)
            (set fn-name (compiler.gensym scope))))

    (let [arg-list (compiler.assert (utils.is-table (. ast index))
                                   "expected parameters"
                                   (if (= (type (. ast index)) "table")
                                       (. ast index) ast))]
      (fn get-arg-name [i name]
        (if (utils.is-varg name)
            (do (compiler.assert (= i (# arg-list))
                                 "expected vararg as last parameter" (. ast 2))
                (set f-scope.vararg true)
                "...")
            (and (utils.is-sym name)
                 (not= (utils.deref name) "nil")
                 (not (utils.is-multi-sym (utils.deref name))))
            (compiler.declare-local name [] f-scope ast)
            (utils.is-table name)
            (let [raw (utils.sym (compiler.gensym scope))
                  declared (compiler.declare-local raw [] f-scope ast)]
              (compiler.destructure name raw ast f-scope f-chunk {:declaration true
                                                                :nomulti true})
              declared)
            (compiler.assert false
                             (: "expected symbol for function parameter: %s"
                                :format (tostring name)) (. ast 2))))
      (local arg-name-list (utils.kvmap arg-list get-arg-name))
      (when (and (= (type (. ast (+ index 1))) "string") (< (+ index 1) (# ast)))
        (set index (+ index 1))
        (set docstring (. ast index)))

      (for [i (+ index 1) (# ast) 1]
        (compiler.compile1 (. ast i) f-scope f-chunk
                           {:nval (or (and (not= i (# ast)) 0) nil)
                            :tail (= i (# ast))}))
      (if is-local-fn
          (compiler.emit parent (: "local function %s(%s)" :format
                                   fn-name (table.concat arg-name-list ", ")) ast)
          (compiler.emit parent (: "%s = function(%s)" :format
                                   fn-name (table.concat arg-name-list ", ")) ast))
      (compiler.emit parent f-chunk ast)
      (compiler.emit parent "end" ast)
      (when utils.root.options.useMetadata
        ;; TODO: show destructured args properly instead of replacing
        (let [args (utils.map arg-list (fn [v] (if (utils.is-table v)
                                                  "\"#<table>\""
                                                  (: "\"%s\"" :format
                                                     (tostring v)))))
              meta-fields ["\"fnl/arglist\"" (.. "{" (table.concat args ", ") "}")]]
          (when docstring
            (table.insert meta-fields "\"fnl/docstring\"")
            (table.insert meta-fields (.. "\"" (-> docstring
                                                  (: :gsub "%s+$" "")
                                                  (: :gsub "\\" "\\\\")
                                                  (: :gsub "\n" "\\n")
                                                  (: :gsub "\"" "\\\"")) "\"")))
          (let [meta-str (: "require(\"%s\").metadata"
                           :format (or utils.root.options.moduleName "fennel"))]
            (compiler.emit parent (: "pcall(function() %s:setall(%s, %s) end)"
                                     :format meta-str fn-name
                                     (table.concat meta-fields ", ")))))))
    (utils.expr fn-name "sym")))

(doc-special "fn" ["name?" "args" "docstring?" "..."]
            (.. "Function syntax. May optionally include a name and docstring.
If a name is provided, the function will be bound in the current scope.
When called with the wrong number of args, excess args will be discarded
and lacking args will be nil, use lambda for arity-checked functions."))

;; FORBIDDEN KNOWLEDGE:
;; (lua "print('hello!')") -> prints hello, evaluates to nil
;; (lua "print 'hello!'" "10") -> prints hello, evaluates to the number 10
;; (lua nil "{1,2,3}") -> Evaluates to a table literal
(fn SPECIALS.lua [ast _ parent]
  (compiler.assert (or (= (# ast) 2) (= (# ast) 3))
                   "expected 1 or 2 arguments" ast)
  (when (not= (. ast 2) nil)
    (table.insert parent {:ast ast :leaf (tostring (. ast 2))}))
  (when (= (# ast) 3)
    (tostring (. ast 3))))

(fn SPECIALS.doc [ast scope parent]
  (assert utils.root.options.useMetadata
          "can't look up doc with metadata disabled.")
  (compiler.assert (= (# ast) 2) "expected one argument" ast)
  (let [target (utils.deref (. ast 2))
        special-or-macro (or (. scope.specials target) (. scope.macros target))]
    (if special-or-macro
        (: "print([[%s]])" :format (doc* special-or-macro target))
        (let [value (tostring (. (compiler.compile1 (. ast 2)
                                                    scope parent {:nval 1}) 1))]
          ;; need to require here since the metadata is stored in the module
          ;; and we need to make sure we look it up in the same module it was
          ;; declared from.
          (: "print(require('%s').doc(%s, '%s'))" :format
             (or utils.root.options.moduleName "fennel") value
             (tostring (. ast 2)))))))

(doc-special
 "doc" ["x"]
 "Print the docstring and arglist for a function, macro, or special form.")

(fn dot [ast scope parent]
  "Table lookup; equivalent to tbl[] in Lua."
  (compiler.assert (< 1 (# ast)) "expected table argument" ast)
  (let [len (# ast)
        lhs (compiler.compile1 (. ast 2) scope parent {:nval 1})]
    (if (= len 2)
        (tostring (. lhs 1))
        (let [indices []]
          (for [i 3 len 1]
            (var index (. ast i))
            (if (and (= (type index) "string")
                     (utils.is-valid-lua-identifier index))
                (table.insert indices (.. "." index))
                (do
                  (set index (. (compiler.compile1 index scope parent {:nval 1})
                                1))
                  (table.insert indices (.. "[" (tostring index) "]")))))
          ;; Extra parens are needed for table literals.
          (if (utils.is-table (. ast 2))
              (.. "(" (tostring (. lhs 1)) ")" (table.concat indices))
              (.. (tostring (. lhs 1)) (table.concat indices)))))))

(tset SPECIALS "." dot)

(doc-special
 "." ["tbl" "key1" "..."]
 "Look up key1 in tbl table. If more args are provided, do a nested lookup.")

(fn SPECIALS.global [ast scope parent]
  (compiler.assert (= (# ast) 3) "expected name and value" ast)
  (compiler.destructure (. ast 2) (. ast 3) ast scope parent {:forceglobal true
                                                              :nomulti true}))

(doc-special "global" ["name" "val"] "Set name as a global with val.")

(fn SPECIALS.set [ast scope parent]
  (compiler.assert (= (# ast) 3) "expected name and value" ast)
  (compiler.destructure (. ast 2) (. ast 3) ast scope parent {:noundef true}))

(doc-special
 "set" ["name" "val"]
 "Set a local variable to a new value. Only works on locals using var.")

(fn set-forcibly!* [ast scope parent]
  (compiler.assert (= (# ast) 3) "expected name and value" ast)
  (compiler.destructure (. ast 2) (. ast 3) ast scope parent {:forceset true}))
(tset SPECIALS :set-forcibly! set-forcibly!*)

(fn local* [ast scope parent]
  (compiler.assert (= (# ast) 3) "expected name and value" ast)
  (compiler.destructure (. ast 2) (. ast 3) ast scope parent {:declaration true
                                                              :nomulti true}))
(tset SPECIALS "local" local*)

(doc-special "local" ["name" "val"] "Introduce new top-level immutable local.")

(fn SPECIALS.var [ast scope parent]
  (compiler.assert (= (# ast) 3) "expected name and value" ast)
  (compiler.destructure (. ast 2) (. ast 3) ast scope parent {:declaration true
                                                              :isvar true
                                                              :nomulti true}))

(doc-special "var" ["name" "val"] "Introduce new mutable local.")


(fn SPECIALS.let [ast scope parent opts]
  (let [bindings (. ast 2)
        pre-syms []]
    (compiler.assert (or (utils.is-list bindings) (utils.is-table bindings))
                     "expected binding table" ast)
    (compiler.assert (= (% (# bindings) 2) 0)
                     "expected even number of name/value bindings" (. ast 2))
    (compiler.assert (>= (# ast) 3) "expected body expression" (. ast 1))
    ;; we have to gensym the binding for the let body's return value before
    ;; compiling the binding vector, otherwise there's a possibility to conflict
    (for [_ 1 (or opts.nval 0) 1]
      (table.insert pre-syms (compiler.gensym scope)))
    (let [sub-scope (compiler.make-scope scope)
          sub-chunk []]
      (for [i 1 (# bindings) 2]
        (compiler.destructure (. bindings i) (. bindings (+ i 1))
                              ast sub-scope sub-chunk {:declaration true
                                                     :nomulti true}))
      (SPECIALS.do ast scope parent opts 3 sub-chunk sub-scope pre-syms))))

(doc-special
 "let" ["[name1 val1 ... nameN valN]" "..."]
 "Introduces a new scope in which a given set of local bindings are used.")

(fn SPECIALS.tset [ast scope parent]
  "For setting items in a table."
  (compiler.assert (> (# ast) 3) "expected table, key, and value arguments" ast)
  (let [root (. (compiler.compile1 (. ast 2) scope parent {:nval 1}) 1)
        keys []]
    (for [i 3 (- (# ast) 1) 1]
      (let [key (. (compiler.compile1 (. ast i) scope parent {:nval 1}) 1)]
        (tset keys (+ (# keys) 1) (tostring key))))
    (let [value (. (compiler.compile1 (. ast (# ast)) scope parent {:nval 1}) 1)
          rootstr (tostring root)
          ;; Prefix 'do end ' so parens are not ambiguous (grouping or fn call?)
          fmtstr (if (: rootstr :match "^{") "do end (%s)[%s] = %s" "%s[%s] = %s")]
      (compiler.emit parent (: fmtstr :format (tostring root)
                               (table.concat keys "][") (tostring value)) ast))))

(doc-special
 "tset" ["tbl" "key1" "..." "keyN" "val"]
 "Set the value of a table field. Can take additional keys to set
nested values, but all parents must contain an existing table.")

(fn if* [ast scope parent opts]
  (let [do-scope (compiler.make-scope scope)
        branches []
        has-else (and (> (# ast) 3) (= (% (# ast) 2) 0))]
    (var else-branch nil)
    ;; Calculate some external stuff. Optimizes for tail calls and what not
    (var (wrapper inner-tail inner-target target-exprs) (values))
    (if (or opts.tail opts.target opts.nval)
        (if (and opts.nval (not= opts.nval 0) (not opts.target))
            (let [accum []]
              ;; We need to create a target
              (set target-exprs [])
              (for [i 1 opts.nval 1]
                (let [s (compiler.gensym scope)]
                  (tset accum i s)
                  (tset target-exprs i (utils.expr s "sym"))))
              (set (wrapper inner-tail inner-target)
                   (values "target" opts.tail (table.concat accum ", "))))
            (set (wrapper inner-tail inner-target)
                 (values "none" opts.tail opts.target)))
        (set (wrapper inner-tail inner-target) (values "iife" true nil)))

    ;; compile bodies and conditions
    (local body-opts {:nval opts.nval :tail inner-tail :target inner-target})
    (fn compile-body [i]
      (let [chunk []
            cscope (compiler.make-scope do-scope)]
        (compiler.keep-side-effects (compiler.compile1 (. ast i) cscope chunk
                                                     body-opts) chunk nil
                                                     (. ast i))
        {:chunk chunk :scope cscope}))

    (for [i 2 (- (# ast) 1) 2]
      (let [condchunk []
            res (compiler.compile1 (. ast i) do-scope condchunk {:nval 1})
            cond (. res 1)
            branch (compile-body (+ i 1))]
        (set branch.cond cond)
        (set branch.condchunk condchunk)
        (set branch.nested (and (not= i 2) (= (next condchunk nil) nil)))
        (table.insert branches branch)))

    (when has-else
      (set else-branch (compile-body (# ast))))

    ;; Emit code
    (let [s (compiler.gensym scope)
          buffer []]
      (var last-buffer buffer)
      (for [i 1 (# branches)]
        (let [branch (. branches i)
              fstr (if (not branch.nested) "if %s then" "elseif %s then")
              cond (tostring branch.cond)
              cond-line (if (and (= cond :true) branch.nested (= i (# branches)))
                           :else
                           (: fstr :format cond))]
          (if branch.nested
              (compiler.emit last-buffer branch.condchunk ast)
              (each [_ v (ipairs branch.condchunk)]
                (compiler.emit last-buffer v ast)))
          (compiler.emit last-buffer cond-line ast)
          (compiler.emit last-buffer branch.chunk ast)
          (if (= i (# branches))
              (do
                (if has-else
                    (do (compiler.emit last-buffer "else" ast)
                        (compiler.emit last-buffer else-branch.chunk ast))
                    ;; TODO: Consolidate use of cond-line ~= "else" with has-else
                    (and inner-target (not= cond-line "else"))
                    (do (compiler.emit last-buffer "else" ast)
                        (compiler.emit last-buffer (: "%s = nil" :format
                                                     inner-target) ast)))
                (compiler.emit last-buffer "end" ast))
              (not (. (. branches (+ i 1)) "nested"))
              (let [next-buffer []]
                (compiler.emit last-buffer "else" ast)
                (compiler.emit last-buffer next-buffer ast)
                (compiler.emit last-buffer "end" ast)
                (set last-buffer next-buffer)))))
      (if (= wrapper "iife")
          (let [iifeargs (or (and scope.vararg "...") "")]
            (compiler.emit parent (: "local function %s(%s)" :format
                                     (tostring s) iifeargs) ast)
            (compiler.emit parent buffer ast)
            (compiler.emit parent "end" ast)
            (utils.expr (: "%s(%s)" :format (tostring s) iifeargs) :statement))
          (= wrapper "none") ; Splice result right into code
          (do (for [i 1 (# buffer) 1]
                (compiler.emit parent (. buffer i) ast))
              {:returned true})
          ;; wrapper is target
          (do (compiler.emit parent (: "local %s" :format inner-target) ast)
              (for [i 1 (# buffer) 1]
                (compiler.emit parent (. buffer i) ast))
              target-exprs)))))

(tset SPECIALS "if" if*)

(doc-special
 "if" ["cond1" "body1" "..." "condN" "bodyN"]
 "Conditional form.
Takes any number of condition/body pairs and evaluates the first body where
the condition evaluates to truthy. Similar to cond in other lisps.")

(fn SPECIALS.each [ast scope parent]
  (compiler.assert (>= (# ast) 3) "expected body expression" (. ast 1))
  (let [binding (compiler.assert (utils.is-table (. ast 2))
                                 "expected binding table" ast)
        iter (table.remove binding (# binding)) ; last item is iterator call
        destructures []
        new-manglings []
        sub-scope (compiler.make-scope scope)]

    (fn destructure-binding [v]
      (if (utils.is-sym v)
          (compiler.declare-local v [] sub-scope ast new-manglings)
          (let [raw (utils.sym (compiler.gensym sub-scope))]
            (tset destructures raw v)
            (compiler.declare-local raw [] sub-scope ast))))

    (let [bind-vars (utils.map binding destructure-binding)
          vals (compiler.compile1 iter sub-scope parent)
          val-names (utils.map vals tostring)
          chunk []]
      (compiler.emit parent (: "for %s in %s do" :format
                               (table.concat bind-vars ", ")
                               (table.concat val-names ", ")) ast)
      (each [raw args (utils.stablepairs destructures)]
        (compiler.destructure args raw ast sub-scope chunk {:declaration true
                                                           :nomulti true}))
      (compiler.apply-manglings sub-scope new-manglings ast)
      (compile-do ast sub-scope chunk 3)
      (compiler.emit parent chunk ast)
      (compiler.emit parent "end" ast))))

(doc-special
 "each" ["[key value (iterator)]" "..."]
 "Runs the body once for each set of values provided by the given iterator.
Most commonly used with ipairs for sequential tables or pairs for  undefined
order, but can be used with any iterator.")

(fn while* [ast scope parent]
  (let [len1 (# parent)
        condition (. (compiler.compile1 (. ast 2) scope parent {:nval 1}) 1)
        len2 (# parent)
        sub-chunk []]
    (if (not= len1 len2)
        ;; compound condition; move new compilation to subchunk
        (do
          (for [i (+ len1 1) len2 1]
            (tset sub-chunk (+ (# sub-chunk) 1) (. parent i))
            (tset parent i nil))
          (compiler.emit parent "while true do" ast)
          (compiler.emit sub-chunk (: "if not %s then break end"
                                     :format (. condition 1)) ast))
        ;; simple condition
        (compiler.emit parent (.. "while " (tostring condition) " do") ast))
    (compile-do ast (compiler.make-scope scope) sub-chunk 3)
    (compiler.emit parent sub-chunk ast)
    (compiler.emit parent "end" ast)))

(tset SPECIALS "while" while*)

(doc-special
 "while" ["condition" "..."]
 "The classic while loop. Evaluates body until a condition is non-truthy.")

(fn for* [ast scope parent]
  (let [ranges (compiler.assert (utils.is-table (. ast 2))
                                "expected binding table" ast)
        binding-sym (table.remove (. ast 2) 1)
        sub-scope (compiler.make-scope scope)
        range-args []
        chunk []]
    (compiler.assert (utils.is-sym binding-sym)
                     (: "unable to bind %s %s" :format
                        (type binding-sym) (tostring binding-sym)) (. ast 2))
    (compiler.assert (>= (# ast) 3)
                     "expected body expression" (. ast 1))
    (for [i 1 (math.min (# ranges) 3) 1]
      (tset range-args i (tostring (. (compiler.compile1 (. ranges i) sub-scope
                                                        parent {:nval 1}) 1))))
    (compiler.emit parent (: "for %s = %s do" :format
                             (compiler.declare-local binding-sym [] sub-scope ast)
                             (table.concat range-args ", ")) ast)
    (compile-do ast sub-scope chunk 3)
    (compiler.emit parent chunk ast)
    (compiler.emit parent "end" ast)))
(tset SPECIALS "for" for*)

(doc-special
 "for" ["[index start stop step?]" "..."]
 "Numeric loop construct.
Evaluates body once for each value between start and stop (inclusive).")

(fn once [val ast scope parent]
  (if (or (= val.type "statement") (= val.type "expression"))
      (let [s (compiler.gensym scope)]
        (compiler.emit parent (: "local %s = %s" :format s (tostring val)) ast)
        (utils.expr s "sym"))
      val))

(fn method-call [ast scope parent]
  (compiler.assert (>= (# ast) 3) "expected at least 2 arguments" ast)
  ;; Compile object
  (var objectexpr (. (compiler.compile1 (. ast 2) scope parent {:nval 1}) 1))
  (var (methodident methodstring) false)
  (if (and (= (type (. ast 3)) "string") (utils.is-valid-lua-identifier (. ast 3)))
      (do
        (set methodident true)
        (set methodstring (. ast 3)))
      (do
        (set methodstring (tostring (. (compiler.compile1 (. ast 3) scope parent
                                                          {:nval 1}) 1)))
        (set objectexpr (once objectexpr (. ast 2) scope parent))))
  (let [args []] ; compile arguments
    (for [i 4 (# ast) 1]
      (let [subexprs (compiler.compile1 (. ast i) scope parent
                                        {:nval (if (not= i (# ast)) 1 nil)})]
        (utils.map subexprs tostring args)))
    (var fstring nil)
    (if (not methodident)
        (do ; make object the first argument
          (table.insert args 1 (tostring objectexpr))
          (set fstring (if (= objectexpr.type "sym")
                           "%s[%s](%s)"
                           "(%s)[%s](%s)")))
        (or (= objectexpr.type "literal") (= objectexpr.type "expression"))
        (set fstring "(%s):%s(%s)")
        (set fstring "%s:%s(%s)"))
    (utils.expr (: fstring :format (tostring objectexpr) methodstring
                   (table.concat args ", ")) "statement")))

(tset SPECIALS ":" method-call)

(doc-special
 ":" ["tbl" "method-name" "..."]
 "Call the named method on tbl with the provided args.
Method name doesn't have to be known at compile-time; if it is, use
(tbl:method-name ...) instead.")

(fn SPECIALS.comment [ast _ parent]
  (let [els []]
    (for [i 2 (# ast) 1]
      (tset els (+ (# els) 1) (: (tostring (. ast i)) :gsub "\n" " ")))
    (compiler.emit parent (.. "-- " (table.concat els " ")) ast)))

(doc-special "comment" ["..."] "Comment which will be emitted in Lua output.")

(fn SPECIALS.hashfn [ast scope parent]
  (compiler.assert (= (# ast) 2) "expected one argument" ast)
  (let [f-scope (doto (compiler.make-scope scope)
                 (tset :vararg false)
                 (tset :hashfn true))
        f-chunk []
        name (compiler.gensym scope)
        symbol (utils.sym name)]
    (compiler.declare-local symbol [] scope ast)
    (var args [])
    (for [i 1 9]
      (tset args i (compiler.declare-local (utils.sym (.. "$" i)) [] f-scope ast)))

    ;; recursively walk the AST, transforming $... into ...
    (fn walker [idx node parent-node]
      (if (and (utils.is-sym node) (= (utils.deref node) "$..."))
          (do
            (tset parent-node idx (utils.varg))
            (set f-scope.vararg true))
          (or (utils.is-list node) (utils.is-table node))))
    (utils.walk-tree (. ast 2) walker)
    ;; compile body
    (compiler.compile1 (. ast 2) f-scope f-chunk {:tail true})
    (var max-used 0)
    (for [i 1 9 1]
      (when (. (. f-scope.symmeta (.. "$" i)) "used")
        (set max-used i)))
    (when f-scope.vararg
      (compiler.assert (= max-used 0)
                       "$ and $... in hashfn are mutually exclusive" ast)
      (set args [(utils.deref (utils.varg))])
      (set max-used 1))
    (local arg-str (table.concat args ", " 1 max-used))
    (compiler.emit parent (: "local function %s(%s)" :format name arg-str) ast)
    (compiler.emit parent f-chunk ast)
    (compiler.emit parent "end" ast)
    (utils.expr name "sym")))

(doc-special "hashfn" ["..."]
            "Function literal shorthand; args are either $... OR $1, $2, etc.")

(fn define-arithmetic-special [name zero-arity unary-prefix lua-name]
  (let [padded-op (.. " " (or lua-name name) " ")]
    (tset SPECIALS name
          (fn [ast scope parent]
            (local len (# ast))
            (if (= len 1)
                (do
                  (compiler.assert (not= zero-arity nil)
                                   "Expected more than 0 arguments" ast)
                  (utils.expr zero-arity "literal"))
                (let [operands []]
                  (for [i 2 len 1]
                    (let [subexprs (compiler.compile1 (. ast i) scope parent
                                                      {:nval (if (= i 1) 1)})]
                      (utils.map subexprs tostring operands)))
                  (if (= (# operands) 1)
                      (if unary-prefix
                          (.. "(" unary-prefix padded-op (. operands 1) ")")
                          (. operands 1))
                      (.. "(" (table.concat operands padded-op) ")")))))))
  (doc-special name ["a" "b" "..."]
              "Arithmetic operator; works the same as Lua but accepts more arguments."))

(define-arithmetic-special "+" "0")
(define-arithmetic-special ".." "''")
(define-arithmetic-special "^")
(define-arithmetic-special "-" nil "")
(define-arithmetic-special "*" "1")
(define-arithmetic-special "%")
(define-arithmetic-special "/" nil "1")
(define-arithmetic-special "//" nil "1")
(define-arithmetic-special "lshift" nil "1" "<<")
(define-arithmetic-special "rshift" nil "1" ">>")
(define-arithmetic-special "band" "0" "0" "&")
(define-arithmetic-special "bor" "0" "0" "|")
(define-arithmetic-special "bxor" "0" "0" "~")

(doc-special "lshift" ["x" "n"]
            "Bitwise logical left shift of x by n bits; only works in Lua 5.3+.")
(doc-special "rshift" ["x" "n"]
            "Bitwise logical right shift of x by n bits; only works in Lua 5.3+.")
(doc-special "band" ["x1" "x2"]
            "Bitwise AND of arguments; only works in Lua 5.3+.")
(doc-special "bor" ["x1" "x2"]
            "Bitwise OR of arguments; only works in Lua 5.3+.")
(doc-special "bxor" ["x1" "x2"]
            "Bitwise XOR of arguments; only works in Lua 5.3+.")

(define-arithmetic-special "or" "false")
(define-arithmetic-special "and" "true")

(doc-special "and" ["a" "b" "..."]
            "Boolean operator; works the same as Lua but accepts more arguments.")
(doc-special "or" ["a" "b" "..."]
            "Boolean operator; works the same as Lua but accepts more arguments.")
(doc-special ".." ["a" "b" "..."]
            "String concatenation operator; works the same as Lua but accepts more arguments.")

(fn define-comparator-special [name realop chain-op]
  (let [op (or realop name)]
    (fn opfn [ast scope parent]
      (local len (# ast))
      (compiler.assert (> len 2) "expected at least two arguments" ast)
      (local lhs (. (compiler.compile1 (. ast 2) scope parent {:nval 1}) 1))
      (var lastval (. (compiler.compile1 (. ast 3) scope parent {:nval 1}) 1))
      (when (> len 3) ; avoid double-eval by adding locals for side-effects
        (set lastval (once lastval (. ast 3) scope parent)))
      (var out (: "(%s %s %s)" :format (tostring lhs) op (tostring lastval)))
      (when (> len 3)
        (for [i 4 len] ; variadic comparison
          (let [nextval (once (. (compiler.compile1 (. ast i)
                                                    scope parent
                                                    {:nval 1}) 1)
                              (. ast i) scope parent)]
            (set out (: (.. out " %s (%s %s %s)") :format (or chain-op "and")
                        (tostring lastval) op (tostring nextval)))
            (set lastval nextval)))
        (set out (.. "(" out ")")))
      out)
    (tset SPECIALS name opfn))
  (doc-special name ["a" "b" "..."]
     "Comparison operator; works the same as Lua but accepts more arguments."))

(define-comparator-special ">")
(define-comparator-special "<")
(define-comparator-special ">=")
(define-comparator-special "<=")
(define-comparator-special "=" "==")
(define-comparator-special "not=" "~=" "or")
(tset SPECIALS "~=" (. SPECIALS "not=")) ; backwards-compatible alias

(fn define-unary-special [op realop]
  (fn opfn [ast scope parent]
    (compiler.assert (= (# ast) 2) "expected one argument" ast)
    (let [tail (compiler.compile1 (. ast 2) scope parent {:nval 1})]
      (.. (or realop op) (tostring (. tail 1)))))
  (tset SPECIALS op opfn))

(define-unary-special "not" "not ")
(doc-special "not" ["x"] "Logical operator; works the same as Lua.")
(define-unary-special "bnot" "~")
(doc-special "bnot" ["x"] "Bitwise negation; only works in Lua 5.3+.")
(define-unary-special "length" "#")
(doc-special "length" ["x"] "Returns the length of a table or string.")

(tset SPECIALS "#" (. SPECIALS "length")) ; backwards-compatible alias

(fn SPECIALS.quote [ast scope parent]
  (compiler.assert (= (# ast) 2) "expected one argument")
  (var (runtime this-scope) (values true scope))
  (while this-scope
    (set this-scope this-scope.parent)
    (when (= this-scope compiler.scopes.compiler)
      (set runtime false)))
  (compiler.do-quote (. ast 2) scope parent runtime))

(doc-special "quote" ["x"]
            "Quasiquote the following form. Only works in macro/compiler scope.")

(fn make-compiler-env [ast scope parent]
  (setmetatable {:_AST ast ; state of compiler
                 :_CHUNK parent
                 :_IS_COMPILER true
                 :_SCOPE scope
                 :_SPECIALS compiler.scopes.global.specials
                 :_VARARG (utils.varg)

                 ;; Useful for macros and meta programming. All of
                 ;; Fennel can be accessed via fennel.myfun, for example
                 ;; (fennel.eval "(print 1)").

                 :fennel utils.fennel-module
                 :unpack unpack

                 ;; AST functions
                 :list utils.list
                 :list? utils.is-list
                 :multi-sym? utils.is-multi-sym
                 :sequence utils.sequence
                 :sequence? utils.is-sequence
                 :sym utils.sym
                 :sym? utils.is-sym
                 :table? utils.is-table
                 :varg? utils.is-varg

                 ;; scoping functions
                 :gensym (fn [] (utils.sym (compiler.gensym
                                            (or compiler.scopes.macro scope))))
                 :get-scope (fn [] compiler.scopes.macro)
                 :in-scope? (fn [symbol]
                              (compiler.assert compiler.scopes.macro
                                               "must call from macro" ast)
                              (. compiler.scopes.macro.manglings
                                 (tostring symbol)))
                 :macroexpand
                 (fn [form]
                   (compiler.assert compiler.scopes.macro
                                    "must call from macro" ast)
                   (compiler.macroexpand form compiler.scopes.macro))}
                {:__index (or _ENV _G)}))

;; have search-module use package.config to process package.path (windows compat)
(local cfg (string.gmatch package.config "([^\n]+)"))
(local (dirsep pathsep pathmark)
       (values (or (cfg) "/") (or (cfg) ";") (or (cfg) "?")))
(local pkg-config {:dirsep dirsep
                  :pathmark pathmark
                  :pathsep pathsep})

(fn escapepat [str]
  "Escape a string for safe use in a Lua pattern."
  (string.gsub str "[^%w]" "%%%1"))

(fn search-module [modulename pathstring]
  (let [pathsepesc (escapepat pkg-config.pathsep)
        pattern (: "([^%s]*)%s" :format pathsepesc pathsepesc)
        no-dot-module (: modulename :gsub "%." pkg-config.dirsep)
        fullpath (.. (or pathstring utils.fennel-module.path) pkg-config.pathsep)]
    (fn try-path [path]
      (let [filename (: path :gsub (escapepat pkg-config.pathmark) no-dot-module)
            filename2 (: path :gsub (escapepat pkg-config.pathmark) modulename)]
        (match (or (io.open filename) (io.open filename2))
          file (do (file:close) filename))))
    (fn find-in-path [start]
      (match (fullpath:match pattern start)
        path (or (try-path path)
                 (find-in-path (+ start (# path) 1)))))
    (find-in-path 1)))

(fn make-searcher [options]
  "This will allow regular `require` to work with Fennel:
table.insert(package.loaders, fennel.searcher)"
  (fn [module-name]
    (let [opts (utils.copy utils.root.options)
          filename (search-module module-name)]
      (each [k v (pairs (or options {}))]
        (tset opts k v))
      (if filename
          (fn [mod-name]
            (utils.fennel-module.dofile filename opts mod-name))))))

(fn macro-globals [env globals]
  (let [allowed (current-global-names env)]
    (each [_ k (pairs (or globals []))]
      (table.insert allowed k))
    allowed))

(fn add-macros [macros* ast scope]
  (compiler.assert (utils.is-table macros*) "expected macros to be table" ast)
  (each [k v (pairs macros*)]
    (compiler.assert (= (type v) "function")
                     "expected each macro to be function" ast)
    (tset scope.macros k v)))

(fn load-macros [modname ast scope parent]
  (let [filename (compiler.assert (search-module modname)
                                  (.. modname " module not found.") ast)
        env (make-compiler-env ast scope parent)
        globals (macro-globals env (current-global-names))]
    (utils.fennel-module.dofile filename {:allowedGlobals globals
                                         :env env
                                         :useMetadata utils.root.options.useMetadata
                                         :scope compiler.scopes.compiler})))

(local macro-loaded [])

(fn SPECIALS.require-macros [ast scope parent]
  (compiler.assert (= (# ast) 2) "Expected one module name argument" ast)
  (let [modname (. ast 2)]
    (when (not (. macro-loaded modname))
      (tset macro-loaded modname (load-macros modname ast scope parent)))
    (add-macros (. macro-loaded modname) ast scope parent)))

(doc-special
 "require-macros" ["macro-module-name"]
 "Load given module and use its contents as macro definitions in current scope.
Macro module should return a table of macro functions with string keys.
Consider using import-macros instead as it is more flexible.")

(fn emit-fennel [src path opts sub-chunk]
  "Emit Fennel code in src into sub-chunk."
  (let [subscope (compiler.make-scope utils.root.scope.parent)
        forms []]
    (when utils.root.options.requireAsInclude
      (set subscope.specials.require compiler.require-include))
    ;; parse Fennel src into table of exprs to know which expr is the tail
    (each [_ val (parser.parser (utils.string-stream src) path)]
      (table.insert forms val))
    ;; Compile the forms into sub-chunk; compiler.compile1 is necessary
    ;; for all nested includes to be emitted in the same root chunk
    ;; in the top-level module.
    (for [i 1 (# forms)]
      (let [subopts (if (= i (# forms)) {:nval 1 :tail true} {:nval 0})]
        (utils.propagate-options opts subopts)
        (compiler.compile1 (. forms i) subscope sub-chunk subopts)))))

(fn include-path [ast opts path mod fennel?]
  "Helper function for include once we have determined the path to use."
  (tset utils.root.scope.includes mod :fnl/loading)
  (let [src (with-open [f (assert (io.open path))]
            (: (f:read "*all") :gsub "[\r\n]*$" ""))
        ;; splice in source and memoize it in compiler AND package.preload
        ;; so we can include it again without duplication, even in runtime
        ret (utils.expr (.. "require(\"" mod "\")") "statement")
        target (: "package.preload[%q]" :format mod)
        preload-str (.. target " = " target " or function(...)")
        (temp-chunk sub-chunk) (values [] [])]
    (compiler.emit temp-chunk preload-str ast)
    (compiler.emit temp-chunk sub-chunk)
    (compiler.emit temp-chunk "end" ast)
    ;; Splice temp-chunk to begining of root chunk
    (each [i v (ipairs temp-chunk)]
      (table.insert utils.root.chunk i v))

    ;; For fennel source, compile sub-chunk AFTER splicing into start of
    ;; root chunk.
    (if fennel?
        (emit-fennel src path opts sub-chunk)
        ;; For Lua source, simply emit src into the loaders's body
        (compiler.emit sub-chunk src ast))

    ;; Put in cache and return
    (tset utils.root.scope.includes mod ret)
    ret))

(fn include-circular-fallback [mod modexpr fallback ast]
  "If a circular include is detected, fall back to require if possible."
  (when (= (. utils.root.scope.includes mod) :fnl/loading) ; circular include
    (compiler.assert fallback "circular include detected" ast)
    (fallback modexpr)))

(fn SPECIALS.include [ast scope parent opts]
  (compiler.assert (= (# ast) 2) "expected one argument" ast)
  (let [modexpr (. (compiler.compile1 (. ast 2) scope parent {:nval 1}) 1)]
    (if (or (not= modexpr.type "literal") (not= (: (. modexpr 1) :byte) 34))
        (if opts.fallback
            (opts.fallback modexpr)
            (compiler.assert false "module name must be string literal" ast))
        (let [mod ((load-code (.. "return " (. modexpr 1))))]
          (or (include-circular-fallback mod modexpr opts.fallback ast)
              (. utils.root.scope.includes mod) ; check cache
              ;; Find path to Fennel or Lua source
              (match (search-module mod) ; try fennel path first
                fennel-path (include-path ast opts fennel-path mod true)
                ;; then search for a lua module
                _ (let [lua-path (search-module mod package.path)]
                    (if lua-path (include-path ast opts lua-path mod false)
                        opts.fallback (opts.fallback modexpr)
                        (compiler.assert false (.. "module not found " mod)
                                         ast)))))))))

(doc-special
 "include" ["module-name-literal"]
 "Like require but load the target module during compilation and embed it in the
Lua output. The module must be a string literal and resolvable at compile time.")

(fn eval-compiler* [ast scope parent]
  (let [scope (compiler.make-scope compiler.scopes.compiler)
        luasrc (compiler.compile ast {:useMetadata utils.root.options.useMetadata
                                   :scope scope})
        loader (load-code luasrc (wrap-env (make-compiler-env ast scope parent)))]
    (loader)))

(fn SPECIALS.macros [ast scope parent]
  (compiler.assert (= (# ast) 2) "Expected one table argument" ast)
  (add-macros (eval-compiler* (. ast 2) scope parent) ast scope parent))

(doc-special
 "macros" ["{:macro-name-1 (fn [...] ...) ... :macro-name-N macro-body-N}"]
 "Define all functions in the given table as macros local to the current scope.")

(fn SPECIALS.eval-compiler [ast scope parent]
  (let [old-first (. ast 1)]
    (tset ast 1 (utils.sym "do"))
    (let [val (eval-compiler* ast scope parent)]
      (tset ast 1 old-first)
      val)))

(doc-special
 "eval-compiler" ["..."]
 "Evaluate the body at compile-time. Use the macro system instead if possible.")

{:doc doc*

 : current-global-names
 : load-code
 : macro-loaded
 : make-compiler-env
 : search-module
 : make-searcher
 : wrap-env}
