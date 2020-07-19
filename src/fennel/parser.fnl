(local utils (require :fennel.utils))
(local friend (require :fennel.friend))
(local fennelview (require :fennelview))
(local {: byte-stream->form-stream} (require :src.fennel.ftb-parser))
(local unpack (or _G.unpack table.unpack))

(fn granulate [getchunk]
  "Convert a stream of chunks to a stream of bytes.
Also returns a second function to clear the buffer in the byte stream"
  (var c "")
  (var index 1)
  (var done false)
  (values (fn [parser-state]
            (when (not done)
              (if (<= index (# c))
                  (let [b (: c "byte" index)]
                    (set index (+ index 1))
                    b)
                  (do
                    (set c (getchunk parser-state))
                    (when (or (not c) (= c ""))
                      (set done true)
                      (lua "return nil"))
                    (set index 2)
                    (: c "byte" 1)))))
          (fn [] (set c ""))))

(fn string-stream [str]
  "Convert a string into a stream of bytes."
  (let [str (: str "gsub" "^#![^\n]*\n" "")] ; remove shebang
    (var index 1)
    (fn [] (local r (: str "byte" index))
      (set index (+ index 1))
      r)))

;; Table of delimiter bytes - (, ), [, ], {, }
;; Opener keys have closer as the value; closers keys have true as their value.
(local delims {40 41 41 true
               91 93 93 true
               123 125 125 true})

(fn iswhitespace [b]
  (or (= b 32) (and (>= b 9) (<= b 13))))

(fn issymbolchar [b]
  (and (> b 32)
       (not (. delims b))
       (not= b 127) ; backspace
       (not= b 34) ; backslash
       (not= b 39) ; single quote
       (not= b 126) ; tilde
       (not= b 59) ; semicolon
       (not= b 44) ; comma
       (not= b 64) ; at
       (not= b 96))) ; backtick

;; prefix chars substituted while reading
(local prefixes {35 "hashfn" ; #
                 39 "quote" ; '
                 44 "unquote" ; ,
                 96 "quote"}); `

(fn parser [getbyte filename options]
  "Parse one value given a function that returns sequential bytes.
Will throw an error as soon as possible without getting more bytes on bad input.
Returns if a value was read, and then the value read. Will return nil when input
stream is finished."
  (let [form-stream (byte-stream->form-stream getbyte)]
    (each [f form-stream]
      (print (fennelview f)))))

{: granulate : parser : string-stream}
