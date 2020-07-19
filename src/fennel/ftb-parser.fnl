(require-macros "src.fennel.enum")
(local {: map-stream} (require "src.fennel.utils"))
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
;; [form-type ... values]
(local form-types (enum symbol string number sequence table list))
(fn escape-string-for-output [str]
  (str:gsub "[\1-\31]" #(.. "\\" ($:byte))))

(fn first-values [first] first)
(fn rest-values [first ...] ...)
(fn map-values [fun item ...]
  (when (not= item nil)
    (values (fun item) (map-values fun ...))))

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
                (match form.type
                  form-types.symbol (. form 1)
                  form-types.number (tostring (. form 1))

                  form-types.string
                  (.. "\"" (escape-string-for-output (. form 1)) "\"")

                  form-types.list (complex-form->string form "(" ")")
                  form-types.table (complex-form->string form "{" "}")
                  form-types.sequence
                  (complex-form->string form "[" "]"))))
         form->string))

(local form-methods
       {:push (fn [form child-form]
                (set form.length (+ form.length 1))
                (tset form form.length child-form)
                child-form)})

(local FORM-MT {:__index form-methods
                :__tostring form->string
                ;; :__fennelview form->string
                })

(fn create-form [form-type ...]
  (let [form [...]]
    (tset form :type form-type)
    (tset form :length (select :# ...))
    (setmetatable form FORM-MT)
    form))

(fn string-form [str]
  (create-form form-types.string (canonicalize str)))
(fn string-form-from-bytes [...] (string-form (string.char ...)))
(fn string-form-from-keyword-string-bytes [colon ...]
  (create-form form-types.string (string.char ...)))
(fn number-form-from-bytes [...]
  (let [substituted (string.gsub (string.char ...) "_" "")]
    (create-form form-types.number (tonumber substituted))))
(fn symbol-form [str] (create-form form-types.symbol str))
(fn symbol-form-from-bytes [...] (symbol-form (string.char ...)))
(fn sequence-form [...] (create-form form-types.sequence ...))
(fn table-form [...] (create-form form-types.table ...))
(fn list-form [...] (create-form form-types.list ...))

(fn open-form-with-stack [stack bytes]
  (stack:push
   (match bytes
     [40] (list-form)
     [91] (sequence-form)
     [123] (table-form))))

(fn open-prefix-form-with-stack [stack bytes]
  (stack:push
   (match bytes
     [35] (list-form (symbol-form :hashfn))
     [44] (list-form (symbol-form :unquote))
     [96] (list-form (symbol-form :quote)))))

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
                (fn [token-type first ...]
                  (values token-type (when first [first ...])))))

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
      (let [(token-type bytes) (boxed-token-stream)]
        (when (and needs-whitespace
                   (not= token-type tts.whitespace)
                   (not= token-type tts.comment)
                   (not= token-type tts.closer))
          (error (.. "expected whitespace, got "
                     (when token-type (. token-types token-type)))))
        (match token-type
          tts.symbol
          (dispatch (symbol-form-from-bytes (unpack bytes)))
          tts.string
          (dispatch (string-form-from-bytes (unpack bytes)))
          tts.keyword-string
          (dispatch (string-form-from-keyword-string-bytes
                     (unpack bytes)))
          tts.number
          (dispatch (number-form-from-bytes (unpack bytes)))
          tts.whitespace nil
          tts.comment nil
          tts.opener (open-form bytes)
          tts.prefix (do (tset prefixes-at (+ stack.length 1) true)
                         (open-prefix-form bytes))
          tts.closer (let [form (close-form bytes)] (dispatch form))
          nil (do (set should-return true) (set return-value nil)))
        (set needs-whitespace (and (not= token-type tts.opener)
                                   (not= token-type tts.prefix)
                                   (not= token-type tts.whitespace)
                                   (not= token-type tts.comment)))
        (if should-return return-value (take-form))))
    take-form))

(fn byte-stream->form-stream [byte-stream]
  (-> byte-stream
      byte-stream->token-stream
      token-stream->form-stream))

(fn string->form-stream [str]
  (-> str
      string->token-stream
      token-stream->form-stream))

{: byte-stream->form-stream : string->form-stream}
