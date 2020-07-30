(require-macros "src.fennel.enum")
(local {: list : sequence : sym : varg : deref : expr
        : isExpr : isList : isMultiSym : isSequence : isSym : isTable : isVarg
        : map-stream : map-values} (require "src.fennel.utils"))
(local {: token-types
        : byte-stream->token-stream
        : string->token-stream} (require "src.fennel.tokenizer"))
(local friend (require :fennel.friend))
(local {: create-stack} (require "src.fennel.stack"))

(global _ENV _ENV)
(global _G _G)
(global setfenv setfenv)
(global loadstring loadstring)

(local unpack (or _G.unpack table.unpack))

(fn load-code [code environment filename]
  (var environment environment)
  (set environment (or environment _ENV _G))
  (let [filename (or filename :anonymous)]
    (if (and setfenv loadstring)
        (let [f (assert (loadstring code filename))]
          (setfenv f environment)
          f)
        (assert (load code filename :t environment)))))

(fn canonicalize [str]
  (let [formatted (str:gsub "[\1-\31]" #(.. "\\" ($1:byte)))
        load-fn (load-code (: "return %s" :format formatted) nil)]
    (load-fn)))

;; a "form" has the following shape:
;; [form-type position ... values]
(local form-types (enum symbol string number sequence table list))
(fn escape-string-for-output [str]
  (str:gsub "[\1-\31]" #(.. "\\" ($:byte))))

(fn concat-strings-with-spaces [first second ...]
  (if (and (not first) (not second)) (values)
      (and first (not second)) first
      (concat-strings-with-spaces (.. first " " second ) ...)))

;; a better way of expressing mutual recursion would clean this up
;; quite a bit
(local form->string
       (do
         (var form->string nil)
         (fn complex-form->string [form opener closer]
           (.. opener
               (or (concat-strings-with-spaces
                    (map-values form->string (unpack form))) "")
               closer))
         (set form->string
              (fn form->string [form]
                (or (match form.type
                      form-types.symbol (. form 1)
                      form-types.number (tostring (. form 1))

                      form-types.string
                      (.. "\"" (escape-string-for-output (. form 1)) "\"")

                      form-types.list (complex-form->string form "(" ")")
                      form-types.table (complex-form->string form "{" "}")
                      form-types.sequence
                      (complex-form->string form "[" "]"))
                    "<unprintable-form>")))
         form->string))

(local form-methods
  {:push (fn [form child-form]
           (set form.length (+ form.length 1))
           (tset form form.length child-form)
           child-form)})

(local FORM-MT {:__index form-methods
                :__tostring form->string
                :__fennelview form->string})

(fn create-form [form-type position ...]
  (let [form [...]]
    (tset form :type form-type)
    (tset form :length (select :# ...))
    (tset form :position position)
    (setmetatable form FORM-MT)
    form))

(fn string-form [position str]
  (create-form form-types.string position (canonicalize str)))
(fn string-form-from-bytes [position ...] (string-form position (string.char ...)))
(fn string-form-from-keyword-string-bytes [position colon ...]
  (create-form form-types.string position (string.char ...)))
(fn number-form-from-bytes [position ...]
  (let [substituted (string.gsub (string.char ...) "_" "")]
    (create-form form-types.number position (tonumber substituted))))
(fn symbol-form [position str] (create-form form-types.symbol position str))
(fn symbol-form-from-bytes [position ...] (symbol-form position (string.char ...)))
(fn sequence-form [position ...] (create-form form-types.sequence position ...))
(fn table-form [position ...] (create-form form-types.table position ...))
(fn list-form [position ...] (create-form form-types.list position ...))

(fn open-form-with-stack [stack position bytes]
  (stack:push
   (match bytes
     [40] (list-form position)
     [91] (sequence-form position)
     [123] (table-form position))))

(fn open-prefix-form-with-stack [stack position bytes]
  (stack:push
   (match bytes
     [35] (list-form position (symbol-form position :hashfn))
     [44] (list-form position (symbol-form position :unquote))
     [96] (list-form position (symbol-form position :quote)))))

(fn close-form-with-stack [stack bytes]
  (when (= stack.length 0)
    (error (.. "unexpected closing delimiter "
               (string.char (unpack bytes)))))
  (let [form (stack:pop)
        expected-closer (match form.type
                          form-types.list ")"
                          form-types.sequence "]"
                          form-types.table "}")
        closer (string.char (unpack bytes))]
    (when (not= expected-closer closer)
      (error (.. "unexpected closing delimiter " closer
                 ", expected " expected-closer)))
    form))

(local parser-states (enum expecting-form
                           expecting-whitespace
                           expecting-prefixed-form))

(local box-tokens
       (partial map-stream
                (fn [token-type position first ...]
                  (when first (values token-type position (when first [first ...]))))))

(fn token-stream->form-stream [token-stream]
  (let [boxed-token-stream (box-tokens token-stream)
        tts token-types
        fts form-types
        stack (create-stack)
        state-stack (create-stack)
        prefixes-at []
        open-form (partial open-form-with-stack stack)
        open-prefix-form (partial open-prefix-form-with-stack stack)
        close-form (partial close-form-with-stack stack)]
    (var needs-whitespace nil)
    (fn take-form []
      (var should-return nil)
      (var return-value nil)
      (fn dispatch [form]
        (if (= stack.length 0)
            (do (set should-return true)
                (set return-value form))
            (let [parent-form (stack:peek)
                  pushed-form (parent-form:push form)]
              (if (. prefixes-at stack.length)
                  (do (tset prefixes-at stack.length nil)
                      (let [further-form (stack:pop)]
                        (dispatch further-form)))
                  pushed-form))))
      (let [(token-type position bytes) (boxed-token-stream)]
        (when (and needs-whitespace
                   (not= token-type tts.whitespace)
                   (not= token-type tts.comment)
                   (not= token-type tts.closer))
          (error (.. "expected whitespace, got "
                     (or (and token-type (. token-types token-type)) ""))))
        (match token-type
          tts.symbol
          (dispatch (symbol-form-from-bytes position (unpack bytes)))
          tts.string
          (dispatch (string-form-from-bytes position (unpack bytes)))
          tts.keyword-string
          (dispatch (string-form-from-keyword-string-bytes position
                     (unpack bytes)))
          tts.number
          (dispatch (number-form-from-bytes position (unpack bytes)))
          tts.whitespace nil
          tts.comment nil
          tts.opener (open-form position bytes)
          tts.prefix (do (tset prefixes-at (+ stack.length 1) true)
                         (open-prefix-form position bytes))
          tts.closer (let [form (close-form bytes)] (dispatch form))
          nil (do (set should-return true) (set return-value nil)))
        (set needs-whitespace (and (not= token-type tts.opener)
                                   (not= token-type tts.prefix)
                                   (not= token-type tts.whitespace)
                                   (not= token-type tts.comment)))
        (if should-return
            return-value
            (take-form))))
    #(take-form)))

(fn byte-stream->form-stream [byte-stream]
  (-> byte-stream
      byte-stream->token-stream
      token-stream->form-stream))

(fn string->form-stream [str]
  (-> str
      string->token-stream
      token-stream->form-stream))

{: byte-stream->form-stream : string->form-stream : form-types}
