(local {: map-stream : granulate} (require :fennel.utils))
(local friend (require :fennel.friend))
(local fennelview (require :fennelview))
(local {: byte-stream->form-stream} (require :src.fennel.ftb-parser))
(local unpack (or _G.unpack table.unpack))

(fn parser [getbyte filename options]
  "Parse one value given a function that returns sequential bytes.
Will throw an error as soon as possible without getting more bytes on bad input.
Returns if a value was read, and then the value read. Will return nil when input
stream is finished."
  (map-stream (fn [form]
                (when form (print (fennelview form)))
                (if form (values true form)
                    false form))
              (byte-stream->form-stream getbyte)))

{: parser : granulate}
