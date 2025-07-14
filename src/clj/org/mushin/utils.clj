(ns org.mushin.utils)

(defn stringify-kw
  "Turn a keyword into a string, preserving namespace."
  [kw]
  (let [ns (namespace kw)]
    (if (nil? ns)
      (name kw)
      (str ns "/" (name kw)))))

(defn concat-kw
  "Concat a keyword and a string."
  [kw s]
  (keyword (str (stringify-kw kw) s)))
