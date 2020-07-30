(local {: list : sequence : sym : varg
        : is-list : is-sym : alternating->kv
        : map-values : map-stream : granulate} (require :fennel.utils))
(local friend (require :fennel.friend))
(local fennelview (require :fennelview))
(local {: byte-stream->form-stream
        : form-types} (require :fennel.ftb-parser))
(local unpack (or _G.unpack table.unpack))

(fn resolve-colon-symbol-keys-in-alternating-values [filename k v ...]
  (if (and (is-sym k) (= (. k 1) ":") (is-sym v))
      (values (. v 1)
              v
              (resolve-colon-symbol-keys-in-alternating-values filename ...))

      (and (is-sym k) (= (. k 1) ":"))
      (error "the placeholder \":\" must be followed by a symbol")

      k
      (values k v (resolve-colon-symbol-keys-in-alternating-values filename ...))

      (values)))

(fn form->dynamic-ast [form filename]
  (when form
    (match form.type
      form-types.symbol
      (if (= (. form 1) "...") (varg)
          (= (. form 1) "true") true
          (= (. form 1) "false") false
          (sym (. form 1) {:bytestart form.position
                           :byteend (+ form.position form.length)
                           : filename}))
      form-types.number (. form 1)
      form-types.string (. form 1)
      form-types.table
      (let [t (alternating->kv (resolve-colon-symbol-keys-in-alternating-values
                                filename
                                (map-values form->dynamic-ast (unpack form))))]
        (setmetatable t {:bytestart form.position
                         :byteend (+ form.position form.length)
                         : filename})
        t)
      form-types.list (list (map-values form->dynamic-ast (unpack form)))
      form-types.sequence (sequence (map-values form->dynamic-ast (unpack form))))))

(fn parser [getbyte filename options]
  "Parse one value given a function that returns sequential bytes.
Will throw an error as soon as possible without getting more bytes on bad input.
Returns if a value was read, and then the value read. Will return nil when input
stream is finished."
  (->> getbyte
       byte-stream->form-stream
       (map-stream
        #(when $1
           (let [dyn-ast (form->dynamic-ast $1 filename)]
             (values true dyn-ast))))))

{: parser : granulate}
