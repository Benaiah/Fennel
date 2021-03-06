(local l (require :luaunit))
(local fennel (require :fennel))

(local failures {
  "\n\n(+))" "unexpected closing delimiter ) in unknown:3"
  "\n\n(let [f (lambda []\n(local))] (f))" "4: expected name and value"
  "\n\n(let [x.y 9] nil)" "unknown:3: did not expect multi"
  "\n(when)" "Compile error in 'when' unknown:2"
  "((fn [] (require-macros \"test-macros\") (global x1 (->1 99 (+ 31)))))\n      (->1 23 (+ 1))" "unknown global in strict mode"
  "()" "expected a function to call"
  "(789)" "cannot call literal value"
  "(do\n\n\n(each \n[x (pairs {})] (when)))" "when' unknown:5:"
  "(do\n\n\n(each \n[x 34 (pairs {})] 21))" "4: expected iterator symbol"
  "(each [k v (pairs {})] (BAD k v))" "BAD"
  "(f" "expected closing delimiter ) in unknown:1"
  "(fn [12])" "expected symbol for function parameter"
  "(fn [:huh] 4)" "expected symbol for function parameter"
  "(fn []\n(for [32 34 32] 21))" "2: expected iterator symbol"
  "(fn [] [...])" "unexpected vararg"
  "(fn [_x y z] y)" "unused local z"
  "(fn [false] 4)" "expected symbol for function parameter"
  "(fn [nil] 4)" "expected symbol for function parameter"
  "(fn [xx y] y)" "unused local xx"
  "(fn global [] 1)" "overshadowed"
  "(fn global-caller [] (hey))" "unknown global"
  "(fn)" "expected vector arg list"
  "(global + 1)" "overshadowed"
  "(global - 1)" "overshadowed"
  "(global // 1)" "overshadowed"
  "(global 48 :forty-eight)" "unable to bind 48"
  "(global good (fn [] nil)) (good) (BAD)" "BAD"
  "(global let 1)" "overshadowed"
  "(hey)" "unknown global"
  "(lambda [x])" "missing body"
  "(let [:x 1] 1)" "unable to bind"
  "(let [[a & c d] [1 2]] c)" "rest argument in final position"
  "(let [b 9\nq (.)] q)" "2: expected table argument"
  "(let [bl 8 a bcd] nil)" "unknown global"
  "(let [false 1] 9)" "unable to bind false"
  "(let [global 1] 1)" "overshadowed"
  "(let [next #(next $)] print)" "aliased by a local"
  "(let [nil 1] 9)" "unable to bind nil"
  "(let [pairs #(pairs $)] pairs)" "aliased by a local"
  "(let [t []] (set t.47 :forty-seven))" "can't start multisym segment with digit: t.47"
  "(let [t []] (set t.:x :y))" "malformed multisym: t.:x"
  "(let [t []] (set t:.x :y))" "malformed multisym: t:.x"
  "(let [t []] (set t::x :y))" "malformed multisym: t::x"
  "(let [t {:a 1}] (+ t.a BAD))" "BAD"
  "(let [x 1 y 2] y)" "unused local x"
  "(let [x 1 y] 8)" "expected even number of name/value bindings"
  "(let [x 1] (set-forcibly! x 2) (set x 3) x)" "expected local var"
  "(let [x 1])" "missing body"
  "(let [x {:foo (fn [self] self.bar) :bar :baz}] x:foo)" "multisym method calls may only be in call position"
  "(let [x {:y {:foo (fn [self] self.bar) :bar :baz}}] x:y:foo)" "method call must be last component of multisym: x:y:foo"
  "(local 47 :forty-seven)" "unable to bind 47"
  "(local a-b 1) (global [a_b] [2])" "global a_b conflicts with local"
  "(local a-b 1) (global a_b 2)" "global a_b conflicts with local"
  "(local a~b 3)" "illegal character: ~"
  "(local ipairs #(ipairs $))" "aliased by a local"
  "(macros {:m (fn [t] `(each [mykey (pairs ,t)] (print mykey)))}) (m [])" "tried to bind mykey without gensym"
  "(macros {:m (fn [t] `(fn [xabc] (+ xabc 9)))}) ((m 4))" "tried to bind xabc without gensym"
  "(macros {:m (fn [y] `(let [x 1] (+ x ,y)))}) (m 4)" "tried to bind x without gensym"
  "(match [1 2 3] [a & b c] nil)" "rest argument in final position"
  "(not true false)" "expected one argument"
  "(print @)" "illegal character: @"
  "(set [a b c] [1 2 3]) (+ a b c)" "expected local var"
  "(set a 19)" "error in 'set' unknown:1: expected local var a"
  "(set)" "Compile error in 'set' unknown:1: expected name and value"
  "(x(y))" "expected whitespace before opening delimiter ("
  "(x[1 2])" "expected whitespace before opening delimiter ["
})

(fn test-failures []
  (each [code expected-msg (pairs failures)]
    (let [(ok? msg) (pcall fennel.compileString code
                           {:allowedGlobals ["pairs" "next" "ipairs"]
                            :checkUnusedLocals true})]
      (l.assertFalse ok? (.. "Expected compiling " code " to fail."))
      (l.assertStrContains msg expected-msg))))

{: test-failures}
