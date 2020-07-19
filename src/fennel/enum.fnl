(local unpack (or _G.unpack table.unpack))
(fn enum [...]
  (let [cases [...]
        stringed-cases []]
    (each [i case (ipairs cases)]
      (let [stringed-case (tostring case)]
        (tset stringed-cases i (tostring case))))
    `(let [this-enum# [,(unpack stringed-cases)]]
       (each [k# v# (ipairs this-enum#)]
         ;; this-enum.CASE will return the int
         (tset this-enum# v# k#)
         ;; this-enum.case? will check equality with the int
         (tset this-enum# (.. v# :?) #(= $ k#)))
       this-enum#)))

{: enum}
