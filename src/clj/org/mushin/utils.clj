(ns org.mushin.utils
  (:import [java.net URI]
           [java.text BreakIterator]))

(defn to-java-uri
  [uri]
  (URI. (str uri)))

(defn disjoint?
  "Are set1 and set2 disjoint?"
  [set1 set2]
  (let [[smaller larger] (if (< (count set1) (count set2)) [set1 set2] [set2 set1])]
    (every? #(not (contains? larger %)) smaller)))

(defn intersecting?
  "Are set1 and set2 intersecting?"
  [set1 set2]
  (not (disjoint? set1 set2)))

(defmacro contains-one-of?
  [coll & keys]
  (let [c (gensym "col__")]
    (when (seq keys)
      `(let [~c ~coll]
         (or ~@(for [k keys] `(and (contains? ~c ~k)
                                   ~k)))))))

(defn grapheme-count
  ^long
  [^String text]
  (loop [it (doto (BreakIterator/getCharacterInstance)
             (.setText text))
         n 0]
    (if (not= (.next it) BreakIterator/DONE)
      (recur it (inc n))
      n)))

(defn contains-key?
  [coll key]
  (and (contains? coll key)
       key))

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
