(require-macros "src.fennel.enum")
(local {: map-values : string->byte-stream} (require "src.fennel.utils"))
(local {: create-cursor} (require "src.fennel.cursor"))
(local {: create-reader : compose-tagged-readers} (require "src.fennel.readers"))

(local unpack (or _G.unpack table.unpack))

(fn create-string-cursor [s] (-> s string->byte-stream create-cursor))

(local token-types
       (enum string number symbol keyword-string
             opener closer prefix
             whitespace comment))

(fn whitespace? [b]
  (and b (or (= b 32)
             (and (>= b 9) (<= b 13)))))

(fn read-whitespace [peek n]
  (let [n (or n 1)]
    (if (whitespace? (peek n 1))
        (read-whitespace peek (+ n 1))
        (- n 1))))

(local whitespace-reader (create-reader (fn [peek] (read-whitespace peek))))

(fn read-comment [peek n]
  (let [n (or n 1)
        peeked (peek n 1)]
    (if (and (= n 1) (not= peeked 59)) 0
        (or (not peeked)
            (and (not= n 1) (= peeked 10))) (- n 1)
        (read-comment peek (+ n 1)))))

(local comment-reader (create-reader (fn [peek] (read-comment peek))))

(local string-reader-states (enum start base backslash done))
(fn read-string [peek n state]
  (let [n (or n 1)
        s string-reader-states
        state (or state s.start)]
    (if (= n 0) 0 ; n has been explicitly set to 0
        (= state s.done) (- n 1)
        (let [char (peek n 1)
              (new-state override-n)
              (match (values state char)
                (_ nil) (error "unterminated string")
                (s.start 34) s.base
                (s.start _) (values s.start 0)
                ((s.start ?b) ? (not ?b)) (values s.start 0)
                (s.base 92) s.backslash
                (s.base 34) s.done
                (s.base _) s.base
                (s.backslash _) s.base)
              new-n (or override-n (+ n 1))]
          (read-string peek new-n new-state)))))

(local string-reader (create-reader (fn [peek] (read-string peek))))

(local delims {40 41    ;; (
               41 true  ;; )
               91 93    ;; [
               93 true  ;; ]
               123 125  ;; {
               125 true ;; }
               })
(fn delim? [b] (not (not (. delims b))))

(local prefixes {96 :quote 44 :unqote 39 :quote 35 :hashfn})
(local prefix-reader
  (create-reader (fn [peek]
                   (if (and (. prefixes (peek))
                            (let [next-b (peek 2 1)]
                              (not (or (whitespace? next-b)
                                       (= (type (. delims next-b)) :boolean))))) 1
                       0))))

(local opener-reader
  (create-reader (fn [peek]
                   (if (= (type (. delims (peek))) :number) 1
                       0))))

(local closer-reader
  (create-reader (fn [peek]
                   (if (= (. delims (peek)) true) 1
                       0))))

(fn symbol-char? [b]
  (and b
       (> b 32)
       (not (. delims b))
       (not= b 34)  ;; "
       (not= b 39)  ;; '
       (not= b 44)  ;; ,
       (not= b 59)  ;; ;
       (not= b 127) ;; DEL
       ))
(fn digit-char? [b] (and (> b 47) (< b 58)))
(fn disallowed-symbol-starter? [b]
  (or (not (symbol-char? b))
      (digit-char? b)
      ))

(fn read-symbol [peek n]
  (let [n (or n 1)
        char (peek n 1)]
    (if (and (= n 1) (disallowed-symbol-starter? char)) 0
        ;; a colon followed by symbol chars is a keyword string
        (and (= n 1) (= char 58) (symbol-char? (peek (+ n 1) 1))) 0
        (symbol-char? char) (read-symbol peek (+ n 1))
        (- n 1))))

(local symbol-reader (create-reader (fn [peek] (read-symbol peek))))

(fn read-keyword-string [peek n]
  (let [n (or n 1)
        char (peek n 1)]
    (if (and (= n 1) (not= char 58)) 0
        (and (= n 2) (not (symbol-char? char))) 0
        (and (> n 2) (not (symbol-char? char))) (- n 1)
        (read-keyword-string peek (+ n 1)))))

(local keyword-string-reader (create-reader (fn [peek] (read-keyword-string peek))))

(fn hex-letter-digit-char? [b] (or (and (> b 64) (< b 71))
                                   (and (> b 96) (< b 103))))
(fn hex-digit-char? [b] (or (digit-char? b) (hex-letter-digit-char? b)))
(fn exponent-char? [b] (or (= b 69) (= b 101)))
(fn hex-indicator-char? [b] (or (= b 88) (= b 120)))
(fn number-char? [b]
  (or (digit-char? b)
      (= b 46) ; 0
      (= b 95) ; _
      ))

(fn err-unexpected-char [b message]
  (error (.. "malformed number: unexpected char \"" (string.char b) "\" " message)))

(local number-reader-states
  (enum start negate dec-point hex-dec-point
        leading-0 base-hex digit dec-digit
        hex-digit hex-dec-digit
        exp exp-negate exp-digit))

(fn err-unhandled-state-transition [state b]
  (error (.. "unhandled state transition in number parser!\tstate: " (. number-reader-states state)
           "\tbyte: " (or b "<nil>") "\tchar: " (or (string.char b) "<nil>"))))

(fn err-invalid-number-character [state b]
  (error (.. "invalid char in number: " (string.char b) "\tchar value: " b)))

;; takes a state and byte (which can potentially be nil) and returns a
;; new state. returning :end will end the collection loop, ignoring
;; the final byte that the state machine was called with
(fn number-reader-state-machine [state byte]
  (let [s number-reader-states]
    (match (values state byte)

      ;; --- start ---
      (s.start 45) s.negate
      (s.start 46) s.dec-point
      (s.start 48) s.leading-0
      ((s.start b) ? (digit-char? b)) s.digit

      ((s.start b) ? (exponent-char? b))
      (error "malformed number: unexpected leading exponent char")

      ((s.start b) ? (hex-indicator-char? b))
      (error "malformed number: unexpected leading hex indicator char")

      ;; --- negate ---
      (s.negate 46) s.dec-point
      (s.negate 48) s.leading-0
      ((s.negate b) ? (digit-char? b)) s.digit
      (s.negate b) (err-unexpected-char b "following negation char")

      ;; --- dec-point ---
      ((s.dec-point b) ? (exponent-char? b)) s.exp
      ((s.dec-point b) ? (digit-char? b)) s.dec-digit
      (s.dec-point b) (err-unexpected-char b "following decimal point")

      ;; --- hex-dec-point
      ((s.hex-dec-point b) ? (hex-digit-char? b)) s.hex-dec-digit
      (s.hex-dec-point b) (err-unexpected-char b "following decimal point")

      ;; --- leading-0 ---
      (s.leading-0 45) (error "unexpected hyphen following leading zero")
      (s.leading-0 46) s.dec-point
      ((s.leading-0 b) ? (digit-char? b)) s.digit
      ((s.leading-0 b) ? (exponent-char? b)) s.exp
      ((s.leading-0 b) ? (hex-indicator-char? b)) s.base-hex

      ;; --- base-hex ---
      (s.base-hex 46) s.hex-dec-point
      ((s.base-hex b) ? (hex-digit-char? b)) s.hex-digit
      (s.base-hex b) (err-unexpected-char b "following hex indicator char")

      ((s.base-hex ?b) ? (not ?b))
      (error "unexpected end of number following hex indicator char")

      ;; --- digit ---
      (s.digit 45) (error "unexpected hyphen following digit")
      (s.digit 46) s.dec-point
      ((s.digit b) ? (digit-char? b)) s.digit
      ((s.digit b) ? (exponent-char? b)) s.exp

      ((s.digit b) ? (hex-letter-digit-char? b))
      (error "unexpected hex digit in non-hex number")

      ((s.digit b) ? (hex-indicator-char? b))
      (error "unexpected hex indicator char following digit")

      ;; --- dec-digit ---
      (s.dec-digit 46) (error "unexpected second decimal point")
      ((s.dec-digit b) ? (digit-char? b)) s.dec-digit

      ;; reuse s.digit state for all other cases
      (s.dec-digit ?b) (number-reader-state-machine s.digit ?b)

      ;; --- hex-digit ---
      (s.hex-digit 45) (error "unexpected hyphen following digit")
      (s.hex-digit 46) s.hex-dec-point
      ((s.hex-digit b) ? (hex-digit-char? b)) s.hex-digit

      ((s.hex-digit b) ? (hex-indicator-char? b))
      (error "unexpected hex indicator char following digit")

      ;; --- hex-dec-digit ---
      (s.hex-dec-digit 46) (error "unexpected second decimal point")
      ((s.hex-dec-digit b) ? (digit-char? b)) s.hex-dec-digit

      ;; reuse s.hex-digit state for all other cases
      (s.hex-dec-digit ?b) (number-reader-state-machine s.hex-digit ?b)

      ;; --- exp ---
      (s.exp 45) s.exp-negate
      ((s.exp b) ? (digit-char? b)) s.exp-digit
      (s.exp b) (err-unexpected-char b "following exponent char")

      ((s.exp ?b) ? (not ?b))
      (error "unexpected end of number following exponent char")

      ;; --- exp-negate ---
      ((s.exp-negate b) ? (digit-char? b)) s.exp-digit
      (s.exp-negate b) (err-unexpected-char b "following exponent hyphen char")

      ((s.exp-negate ?b) ? (not ?b))
      (error "unexpected end of number following exponent hyphen char")


      ;; --- exp-digit ---
      ((s.exp-digit b) ? (digit-char? b)) s.exp-digit

      (s.exp-digit b)
      (error "unexpected char \"" (string.char b) "\" following exponent digit char")

      ;; ignore underscores
      (ss 95) ss

      ((_ ?b) ? (or (not ?b) (whitespace? ?b) (delim? ?b))) s.end

      ;; catch all other states
      _ (err-invalid-number-character state byte))))

(fn check-for-number [peek]
  (let [b (peek)]
    (or (digit-char? b) ;; leading digits always indicate a number
        (let [b2 (peek 2 1)]
          (or (and (or (= b 45) (= b 46)) (digit-char? b2)) ;; e.g. -1 or .1
              (let [b3 (peek 3 1)]
                (and (= b 45) (= b2 46) (digit-char? b3)))))))) ;; e.g. -.1

(fn read-number [peek n state]
  (let [n (or n 1)]
    (if (and (= n 1) (not (check-for-number peek))) 0
        (let [s number-reader-states
              state (or state s.start)
              char (peek n 1)
              new-state (number-reader-state-machine state char)]
          (if (= new-state s.end) (- n 1)
              (read-number peek (+ n 1) new-state))))))

(local number-reader (create-reader (fn [peek] (read-number peek))))

(local fennel-tagged-reader
  (let [tts token-types]
    (compose-tagged-readers tts.string string-reader
                            tts.number number-reader
                            tts.opener opener-reader
                            tts.closer closer-reader
                            tts.whitespace whitespace-reader
                            tts.comment comment-reader
                            tts.prefix prefix-reader
                            tts.keyword-string keyword-string-reader
                            tts.symbol symbol-reader)))

(fn take-token [cursor]
  (when (cursor.peek)
    (let [n (fennel-tagged-reader.readn cursor.peek)]
      (print "N" n)
      (if (> n 0) (fennel-tagged-reader.read-bytes-tagged cursor)
          (let [(b1 b2 b3) (cursor.peek 3)]
            (error (.. "unrecognized byte sequence [" b1 " " b2 " " b3 "] "
                       "\"" (string.char b1 b2 b3) "\"")))))))

(fn byte-stream->token-stream [bytes-stream]
  (let [cursor (create-cursor bytes-stream)]
    #(let [vals [(take-token cursor)]]
       (print "TOKEN"
              ((require :fennelview)
               [(. token-types (. vals 1))
                (table.concat [(map-values string.char (select 2 (unpack vals)))])]))
       (when (. vals 1) (unpack vals)))))

(fn string->token-stream [str]
  (-> str string->byte-stream byte-stream->token-stream))

{: token-types : byte-stream->token-stream : string->token-stream}
