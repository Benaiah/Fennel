(fn create-cursor [stream]
  ;; We track the current position and the end of the buffer. The
  ;; indices of the buffer items will always be between these two
  ;; numbers. Since we don't move the buffer elements back to the
  ;; beginning at any point, these indices will increase
  ;; monotonically.
  (var position 0)
  (var buffer-end 0)
  (let [;; This is the buffer to store values that were retrieved
        ;; ahead of the cursor position
        buffer []
        buffer-length #(- buffer-end position)
        buffer-get #(. buffer (+ position $))
        buffer-set #(tset buffer (+ position $1) $2)
        buffer-push
        #(let [new-buffer-end (+ buffer-end 1)]
           (tset buffer new-buffer-end $)
           (set buffer-end new-buffer-end))]

    (fn buffer-load-and-get [i]
      (if (= i (+ 1 (buffer-length)))
          (let [new-item (stream)]
            (buffer-push new-item)
            new-item)
          (> i (buffer-length))
          (let [new-item (stream)]
            (buffer-push new-item)
            (buffer-load-and-get i))
          (let [old-item (buffer-get i)]
            old-item)))

    ;; Tail recursive peek lets us peek ahead multiple values without
    ;; allocating a table each time
    (fn peek [a b]
      (let [(i n) (match (values a b)
                    (an-i an-n) (values an-i an-n)
                    (an-n nil) (values 1 an-n)
                    (nil nil) (values 1 1))]
        (when (< n 1) (error "cannot peek at less than one value"))
        (if
         ;; Base case - return the remaining item
         (or (not n) (= n 1))
         (buffer-load-and-get i)
         ;; Otherwise, return the item at i and recursively iterate
         ;; until we've returned all the requested values
         (values (buffer-load-and-get i)
                 (peek (+ i 1) (- n 1))))))

    (fn take [n]
      (let [n (if (= n nil) 1 n)]
        (when (> n 0)
          (values
           (if (> (buffer-length) 0)
               (let [item (buffer-get 1)]
                 (buffer-set 1 nil)
                 (set position (+ position 1))
                 item)
               (do (set position (+ position 1))
                   (set buffer-end (+ buffer-end 1))
                   (stream)))
           (take (- n 1))))))

    {: peek : take}))

{: create-cursor}
