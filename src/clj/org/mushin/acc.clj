;; SPDX-License-Identifier: EPL-1.0

(ns
    ^{:author "Mushin Man",
     :doc "This file defines variants of standard clojure functions that include an
accumulator."}
    org.mushin.acc)

(defn mapv-acc
  "Returns a tuple of an accumulator and the result of applying f to each coll,
  until one of the colls is exhausted. The remaining values are ignored. The function
  `f` should accept number-of-colls + 1 arguments, where the accumulator is always first,
  and return a tuple starting with the accumulator and ending with a result value."
  ([f acc coll]
   (reduce
    (fn [[acc result-vec] v]
      (let [[acc v2] (f acc v)]
        [acc (conj result-vec v2)]))
    [acc []]
    coll))
  ([f acc coll1 & colls]
   (reduce
    (fn [[acc result-vec] & v]
      (let [[acc v2] (apply f acc (first v))]
        [acc (conj result-vec v2)]))
    [acc []]
    (apply map (fn [& args] (vec args)) coll1 colls))))


(defn walk
  "Same as clojure's `walk` but carries an accumulator. The `inner` and `outer` functions
  should now take 2 arguments for the accumulator and the form, and should return a tuple
  with the accumulator in the first column."
  [inner outer acc form]
  (cond
    (list? form)
    (let [[acc v] (mapv-acc inner acc form)]
      (outer acc (with-meta (apply list v) (meta form))))

    (instance? clojure.lang.IMapEntry form)
    (let [[acc k] (inner acc (key form))
          [acc v] (inner acc (val form))]
      (outer acc (clojure.lang.MapEntry/create k v)))

    (seq? form)
    (let [[acc v] (mapv-acc inner acc form)]
      (outer acc (with-meta (seq v) (meta form))))

    (instance? clojure.lang.IRecord form)
    (let [[acc v]
          (reduce
           (fn [[acc r] x]
             (let [[acc v] (inner acc x)]
               [acc (conj r v)]))
           [acc form]
           form)]
      (outer acc v))

    (coll? form)
    (let [[acc v] (mapv-acc inner acc form)]
      (outer acc (into (empty form) v)))

    :else (outer acc form)))

(defn postwalk
  "Same as clojure's `postwalk` but carries an accumulator. The `f` function
  should now take 2 arguments for the accumulator and the form, and should return a tuple
  with the accumulator in the first column."
  [f acc form]
  (walk (partial postwalk f) f acc form))
