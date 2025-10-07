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

(defn icase-comp
  "Compare two string ignoring case."
  [x y]
  (if (and (string? x) (string? y))
    (.equalsIgnoreCase x y)
    (= x y)))

(defmacro do-try
  "Try x. If an exception is thrown, gobble it and return nil."
  [x]
  `(try
    ~x
    (catch Exception ~'ex
      nil)))

(defn condj [v val]
  (cond-> v val (conj val)))
