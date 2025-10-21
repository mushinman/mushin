(ns org.mushin.utils)

(defn disjoint?
  "Are set1 and set2 disjoint?"
  [set1 set2]
  (let [[smaller larger] (if (< (count set1) (count set2)) [set1 set2] [set2 set1])]
    (every? #(not (contains? larger %)) smaller)))

(defn intersecting?
  "Are set1 and set2 intersecting?"
  [set1 set2]
  (not (disjoint? set1 set2)))

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
