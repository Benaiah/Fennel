(local {: split-values-alternating} (require "src.fennel.utils"))

(fn create-reader [readn]
  (fn read-bytes [cursor]
    (let [n (readn cursor.peek)] (cursor.take n)))
  (fn read-string [cursor]
    (-> cursor read-bytes string.char))
  {: readn : read-bytes : read-string})

(fn compose-tagged-readers [...]
  (let [(tags readers) (split-values-alternating ...)
        readn-tagged-inner
        (fn readn-tagged-inner [peek i]
          (let [tag (. tags i) reader (. readers i)]
            (if (= reader nil) nil
                (let [n (reader.readn peek)]
                  (if (> n 0) (values tag n)
                      (readn-tagged-inner peek (+ i 1)))))))
        readn-tagged (fn [peek] (readn-tagged-inner peek 1))
        read-bytes-tagged
        (fn [cursor]
          (let [(tag n) (readn-tagged cursor.peek)]
            (values tag (cursor.take n))))
        read-string-tagged-inner
        (fn [tag ...]
          (values tag (string.char ...)))
        read-string-tagged
        (fn [cursor]
          (read-string-tagged-inner (read-bytes-tagged cursor)))
        readn (fn [peek] (select 2 (readn-tagged peek)))
        {: read-bytes : read-string} (create-reader readn)]
    {: readn
     : read-bytes
     : read-string
     : readn-tagged
     : read-bytes-tagged
     : read-string-tagged}))

{: create-reader : compose-tagged-readers}
