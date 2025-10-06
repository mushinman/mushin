(ns org.mushin.with-disposable)

;; TODO This is modified code from clojure's source so it probably needs
;; to be EPL licensed.

(defmacro ^{:private true} assert-args
  [& pairs]
  `(do (when-not ~(first pairs)
         (throw (IllegalArgumentException.
                  (str (first ~'&form) " requires " ~(second pairs) " in " ~'*ns* ":" (:line (meta ~'&form))))))
     ~(let [more (nnext pairs)]
        (when more
          (list* `assert-args more)))))

(defmacro with-disposable
  "bindings => [name init ...]

  Evaluates body in a try expression with names bound to the values
  of the inits, and a finally clause that calls (.dispose name) on each
  name in reverse order."
  [bindings & body]
  (assert-args
     (vector? bindings) "a vector for its binding"
     (even? (count bindings)) "an even number of forms in binding vector")
  (cond
    (= (count bindings) 0) `(do ~@body)
    (symbol? (bindings 0)) `(let ~(subvec bindings 0 2)
                              (try
                                (with-open ~(subvec bindings 2) ~@body)
                                (finally
                                  (. ~(bindings 0) dispose))))
    :else (throw (IllegalArgumentException.
                   "with-open only allows Symbols in bindings"))))
