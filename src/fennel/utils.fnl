(local unpack (or _G.unpack table.unpack))

(fn stablepairs [t]
  "Like pairs, but gives consistent ordering every time. On 5.1, 5.2, and LuaJIT
  pairs is already stable, but on 5.3 every run gives different ordering."
  (let [keys []
        succ []]
    (each [k (pairs t)]
      (table.insert keys k))
    (table.sort keys (fn [a b] (< (tostring a) (tostring b))))
    (each [i k (ipairs keys)]
      (tset succ k (. keys (+ i 1))))
    (fn stablenext [tbl idx]
      (if (= idx nil)
          (values (. keys 1) (. tbl (. keys 1)))
          (values (. succ idx) (. tbl (. succ idx)))))
    (values stablenext t nil)))

(fn map [t f out]
  "Map function f over sequential table t, removing values where f returns nil.
Optionally takes a target table to insert the mapped values into."
  (let [out (or out [])
        f (if (= (type f) "function")
              f
              (let [s f] (fn [x] (. x s))))]
    (each [_ x (ipairs t)]
      (match (f x)
        v (table.insert out v)))
    out))

(fn kvmap [t f out]
  "Map function f over key/value table t, similar to above, but it can return a
sequential table if f returns a single value or a k/v table if f returns two.
Optionally takes a target table to insert the mapped values into."
  (let [out (or out [])
        f (if (= (type f) "function")
              f
              (let [s f] (fn [x] (. x s))))]
    (each [k x (stablepairs t)]
      (let [(korv v) (f k x)]
        (when (and korv (not v))
          (table.insert out korv))
        (when (and korv v)
          (tset out korv v))))
    out))

(fn copy [from]
  "Returns a shallow copy of its table argument. Returns an empty table on nil."
  (let [to []]
    (each [k v (pairs (or from []))]
      (tset to k v))
    to))

(fn allpairs [tbl]
  "Like pairs, but if the table has an __index metamethod, it will recurisvely
traverse upwards, skipping duplicates, to iterate all inherited properties"
  (assert (= (type tbl) "table") "allpairs expects a table")
  (var t tbl)
  (let [seen []]
    (fn allpairs-next [_ state]
      (let [(next-state value) (next t state)]
        (if (. seen next-state)
            (allpairs-next nil next-state)
            next-state
            (do (tset seen next-state true)
                (values next-state value))
            (let [meta (getmetatable t)]
              (when (and meta meta.__index)
                (set t meta.__index)
                (allpairs-next t))))))
    allpairs-next))

(fn deref [self]
  "Get the name of a symbol."
  (. self 1))

(var nil-sym nil) ; haven't defined sym yet; create this later

(fn list-to-string [self tostring2]
  (var (safe max) (values [] 0))
  (each [k (pairs self)]
    (when (and (= (type k) "number") (> k max))
      (set max k)))
  (for [i 1 max 1]
    (tset safe i (or (and (= (. self i) nil) nil-sym) (. self i))))
  (.. "(" (table.concat (map safe (or tostring2 tostring)) " " 1 max) ")"))

(local SYMBOL-MT {1 "SYMBOL" :__fennelview deref :__tostring deref})
(local EXPR-MT {1 "EXPR" :__tostring deref})
(local LIST-MT {1 "LIST" :__fennelview list-to-string :__tostring list-to-string})
(local SEQUENCE-MARKER ["SEQUENCE"])
(local VARARG (setmetatable ["..."]
                            {1 "VARARG" :__fennelview deref :__tostring deref}))

(local getenv (or (and os os.getenv) (fn [] nil)))

(fn debug-on [flag]
  (let [level (or (getenv "FENNEL_DEBUG") "")]
    (or (= level "all") (: level "find" flag))))

(fn list [...]
  "Create a new list. Lists are a compile-time construct in Fennel; they are
represented as tables with a special marker metatable. They only come from
the parser, and they represent code which comes from reading a paren form;
they are specifically not cons cells."
  (setmetatable [...] LIST-MT))

(fn sym [str scope source]
  "Create a new symbol. Symbols are a compile-time construct in Fennel and are
not exposed outside the compiler. Symbols have source data describing what
file, line, etc that they came from."
  (let [s {:scope scope 1 str}]
    (each [k v (pairs (or source []))]
      (when (= (type k) "string")
        (tset s k v)))
    (setmetatable s SYMBOL-MT)))

(set nil-sym (sym "nil"))

(fn sequence [...]
  "Create a new sequence. Sequences are tables that come from the parser when
it encounters a form with square brackets. They are treated as regular tables
except when certain macros need to look for binding forms, etc specifically."
  ;; can't use SEQUENCE-MT directly as the sequence metatable like we do with
  ;; the other types without giving up the ability to set source metadata
  ;; on a sequence, (which we need for error reporting) so embed a marker
  ;; value in the metatable instead.
  (setmetatable [...] {:sequence SEQUENCE-MARKER}))

(fn expr [strcode etype]
  "Create a new expression. etype should be one of:
  :literal literals like numbers, strings, nil, true, false
  :expression Complex strings of Lua code, may have side effects, etc
              but is an expression
  :statement Same as expression, but is also a valid statement (function calls)
  :vargs varargs symbol
  :sym symbol reference"
  (setmetatable {:type etype 1 strcode} EXPR-MT))

(fn varg [] VARARG)

;; TODO: rename these to expr?, varg?, etc once all callers are fennelized
(fn is-expr [x]
  "Checks if an object is an expression. Returns the object if it is."
  (and (= (type x) "table") (= (getmetatable x) EXPR-MT) x))

(fn is-varg [x]
  "Checks if an object is the vararg symbol. Returns the object if is."
  (and (= x VARARG) x))

(fn is-list [x]
  "Checks if an object is a list. Returns the object if is."
  (and (= (type x) "table") (= (getmetatable x) LIST-MT) x))

(fn is-sym [x]
  "Checks if an object is a symbol. Returns the object if it is."
  (and (= (type x) "table") (= (getmetatable x) SYMBOL-MT) x))

(fn is-table [x]
  "Checks if an object any kind of table, EXCEPT list or symbol or vararg."
  (and (= (type x) "table")
       (not= x VARARG)
       (not= (getmetatable x) LIST-MT)
       (not= (getmetatable x) SYMBOL-MT)
       x))

(fn is-sequence [x]
  "Checks if an object is a sequence (created with a [] literal)"
  (let [mt (and (= (type x) "table") (getmetatable x))]
    (and mt (= mt.sequence SEQUENCE-MARKER) x)))

(fn is-multi-sym [str]
  "A multi symbol is a symbol that is actually composed of two or more symbols
using dot syntax. The main differences from normal symbols is that they can't
be declared local, and they may have side effects on invocation (metatables)."
  (if (is-sym str) (is-multi-sym (tostring str))
      (not= (type str) "string") false
      (let [parts []]
        (each [part (str:gmatch "[^%.%:]+[%.%:]?")]
          (local last-char (: part "sub" (- 1)))
          (when (= last-char ":")
            (set parts.multi-sym-method-call true))
          (if (or (= last-char ":") (= last-char "."))
              (tset parts (+ (# parts) 1) (: part "sub" 1 (- 2)))
              (tset parts (+ (# parts) 1) part)))
        (and (> (# parts) 0) (or (: str "match" "%.") (: str "match" ":"))
             (not (: str "match" "%.%."))
             (not= (: str "byte") (string.byte "."))
             (not= (: str "byte" (- 1)) (string.byte "."))
             parts))))

(fn is-quoted [symbol] symbol.quoted)

(fn walk-tree [root f custom-iterator]
  "Walks a tree (like the AST), invoking f(node, idx, parent) on each node.
When f returns a truthy value, recursively walks the children."
  (fn walk [iterfn parent idx node]
    (when (f idx node parent)
      (each [k v (iterfn node)]
        (walk iterfn node k v))))
  (walk (or custom-iterator pairs) nil nil root)
  root)

(local lua-keywords ["and" "break" "do" "else" "elseif" "end" "false" "for"
                    "function" "if" "in" "local" "nil" "not" "or" "repeat"
                    "return" "then" "true" "until" "while"])

(each [i v (ipairs lua-keywords)]
  (tset lua-keywords v i))

(fn is-valid-lua-identifier [str]
  (and (str:match "^[%a_][%w_]*$") (not (. lua-keywords str))))

(local propagated-options [:allowedGlobals :indent :correlate :useMetadata :env])

(fn propagate-options [options subopts]
  "Certain options should always get propagated onwards when a function that
has options calls down into compile."
  (each [_ name (ipairs propagated-options)]
    (tset subopts name (. options name)))
  subopts)

(local root {:chunk nil :scope nil :options nil :reset (fn [])})

(fn root.set-reset [root]
  (let [{: chunk : scope : options : reset} root]
    (fn root.reset []
      #(set (root.chunk root.scope root.options root.reset)
            (values chunk scope options reset)))))

(local split-values-alternating
  (do
    ;; We use a do block to keep split-values-alternating-recursively
    ;; from being visible outside of split-values-alternating.
    (fn split-values-alternating-recursively [odds evens i odd even ...]
      (tset odds i odd)
      (tset evens i even)
      (if (> (select :# ...) 0)
          ;; If there are arguments left, recurse.
          (split-values-alternating-recursively odds evens (+ i 1) ...)

          ;; If there are no arguments left, return the two tables of
          ;; odd and even arguments.
          (values odds evens)))

    ;; This function allows us to set default arguments for
    ;; split-values-alternating-recursively to use for the first
    ;; iteration.
    (fn [...] (split-values-alternating-recursively [] [] 1 ...))))

(fn split-alternating [tab]
  ;; technically, this will break if the table is big enough
  (split-values-alternating (unpack tab)))

(fn string->byte-stream [str]
  (var index 1)
  #(let [r (str:byte index)]
     (set index (+ index 1))
     r))

(fn map-stream [f stream] (fn [...] (f (stream ...))))

(fn chunk-stream->byte-stream [chunk-stream]
  (var chunk "")
  (var index 1)
  (var done false)
  (fn [...]
    (if done nil
        (<= index (length chunk))
        (let [byte (chunk:byte index)]
          (set index (+ index 1))
          byte)

        (do (set chunk (chunk-stream ...))
            (if (or (not chunk) (= chunk ""))
                (set done true)

                (do (set index 2)
                    (chunk:byte 1)))))))

{;; general table functions
 : allpairs : stablepairs : copy : kvmap : map : walk-tree

 ;; AST functions
 : list : sequence : sym : varg : deref : expr : is-quoted
 : is-expr : is-list : is-multi-sym : is-sequence : is-sym : is-table : is-varg

 ;; other
 : is-valid-lua-identifier : lua-keywords
 : propagate-options : root : debug-on
 :path (table.concat (doto ["./?.fnl" "./?/init.fnl"]
                       (table.insert (getenv "FENNEL_PATH"))) ";")
 : split-values-alternating : split-alternating
 : string->byte-stream :stringStream string->byte-stream
 : chunk-stream->byte-stream :granulate chunk-stream->byte-stream
 : map-stream
}
