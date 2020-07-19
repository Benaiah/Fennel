(local stack-methods
  {:push (fn [stack empty-form]
           (set stack.length (+ stack.length 1))
           (tset stack stack.length empty-form)
           empty-form)
   :pop (fn [stack]
          (when (= stack.length 0)
            (error "cannot pop stack with length 0"))
          (let [form (. stack stack.length)]
            (tset stack stack.length nil)
            (set stack.length (- stack.length 1))
            form))
   :peek (fn [stack]
           (when (not= stack.length 0)
             (. stack stack.length)))})

(local STACK-MT {:__index stack-methods})

(fn create-stack []
  (local stack [])
  (tset stack :length 0)
  (setmetatable stack STACK-MT)
  stack)

{: create-stack}
