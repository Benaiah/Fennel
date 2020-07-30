(local {: list : sequence : sym : varg
        : isList
        : map-values : map-stream : granulate} (require :fennel.utils))
(local friend (require :fennel.friend))
(local fennelview (require :fennelview))
(local {: byte-stream->form-stream
        : form-types} (require :fennel.ftb-parser))
(local unpack (or _G.unpack table.unpack))

(fn table-with-metatable [MT ...]
  (let [t [...]]
    (setmetatable t MT)
    t))

(fn form->dynamic-ast [form filename]
  ;; (print "form->dynast" (fennelview form {:metamethod? false}))
  (when form
    ;; (print "FORM TYPE" (. form-types form.type))
    ;; (print "FORM POSITION" (. form-types form.position))
    (match form.type
      form-types.symbol (if (= (. form 1) "...") (varg)
                            (sym (. form 1) {:bytestart form.position
                                             :byteend (+ form.position form.length)
                                             : filename}))
      form-types.number (. form 1)
      form-types.string (. form 1)
      form-types.table [(map-values form->dynamic-ast (unpack form))]
      form-types.list (list (map-values form->dynamic-ast (unpack form)))
      form-types.sequence (sequence (map-values form->dynamic-ast (unpack form))))
    ))

(fn parser [getbyte filename options]
  "Parse one value given a function that returns sequential bytes.
Will throw an error as soon as possible without getting more bytes on bad input.
Returns if a value was read, and then the value read. Will return nil when input
stream is finished."
  (->> getbyte
       byte-stream->form-stream
       (map-stream
        #(when $1
           ;; (print "FORM" $1)
           (let [dyn-ast (form->dynamic-ast $1 filename)]
             (values true dyn-ast))))))

{: parser : granulate}
