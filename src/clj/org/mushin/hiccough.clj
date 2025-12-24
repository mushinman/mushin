(ns org.mushin.hiccough
  (:require [org.mushin.acc :refer [postwalk-noassoc mapv-acc]]
            [clojure.string :as cstr])
  (:import [java.util.regex Matcher]))

(def mentions-regex
  "Regex for matching mentions (e.g. @user)."
  ;; Adapted from the java regex here: https://emailregex.com/
  #"(?i)(?:\B@[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*|\"(?:[\x01-\x08\x0b\x0c\x0e-\x1f\x21\x23-\x5b\x5d-\x7f]|\\[\x01-\x09\x0b\x0c\x0e-\x7f])*\")")

;; TODO uncomment this when federation is implemented.
;(def mentions-regex
 ;"Regex for matching mentions (e.g. @user@domain.tld or @user)."
  ;; Adapted from the java regex here: https://emailregex.com/
;  #"(?i)(?:\B@[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*|\"(?:[\x01-\x08\x0b\x0c\x0e-\x1f\x21\x23-\x5b\x5d-\x7f]|\\[\x01-\x09\x0b\x0c\x0e-\x7f])*\")(@(?:(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?|\[(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?|[a-z0-9-]*[a-z0-9]:(?:[\x01-\x08\x0b\x0c\x0e-\x1f\x21-\x5a\x53-\x7f]|\\[\x01-\x09\x0b\x0c\x0e-\x7f])+)\]))?")

(defn- split-by-mentions
  "Split a string into a vector of strings and maps based off of @ mentions."
  [s]
  (let [matcher ^Matcher (re-matcher mentions-regex s)]
    (loop [result []
           prev-end 0]
      (if-not (.find matcher)
        (if-let [tail (not-empty (subs s prev-end))]
          (conj result tail)
          result)
        (recur (into result
                     (conj (if-let [before-match (not-empty (subs s prev-end (.start matcher)))]
                             [before-match]
                             [])
                           {:match :ap
                            ;; Target username.
                            :target (subs s (+ 1 (.start matcher)) (.end matcher))
                            :original-text (subs s (.start matcher) (.end matcher))}))
               (.end matcher))))))

(defn- tag?
  "Determine if `e` is a SVG tag in hiccough syntax.

  # Arguments
    - `e`: Object to determine if it is an SVG tag or not.

  # Return value
  `true` if `e` is a tag, `false` if not."
  [e]
  (and (vector? e) (keyword? (first e))))

(def default-hiccough-accumulator {:mentions {}})

(defn sanitize-hiccough
  "Sanitize hiccough syntax into valid hiccup. Filters elements and attributes based off
  a whitelist. Convert @ mentions into links. Returns tuple of information relating to
  the hiccup and sanitized hiccup.

  # Arguments
  - `hiccough`: Hiccough syntax to sanitize
  - `whitelist`: A function that takes 1 argument: a hiccup tag (e.g. `:p`) and returns
  `nil` if the tag is not whitelisted, else returns a function. That function takes
  2 arguments: the tag and a map of attributes, and should return a tag with a new
  attributes map, or just a tag, or nil of the tag should be removed.
  - `ids-and-classes?`: True if hiccough should allow hiccup style ids and classes
  (e.g. `:p#id.class`), false if not. If false: all ids and classes are removed.
  - `account-name-to-link:` A function of one argument: an account nickname or a
  fully qualified name (e.g. `nickname` or `nickname@domain.org`), and returns a URI
  to the account resource.

  # Return
  Returns a tuple. In the first column is extra information about the hiccough.
  The sanitized hiccough is in the second column.

  The extra information is of the following form:
  | Key         | Type                  | Value                                                                                              |
  |:------------|:----------------------|:---------------------------------------------------------------------------------------------------|
  | `:mentions` | Map[string -> string] | Map where each key is a nickname or fully qualified name, and each value is a URI to that resource |
  |             |                       |                                                                                                    |
  "
  ([hiccough whitelist ids-and-classes? account-name-to-link initial-acc]
  (postwalk-noassoc
   (fn [{:keys [mentions] :as acc} e]
     (cond
       (or (map? e)
           (keyword? e)
           (number? e)
           (boolean? e)
           (string? e))
       [acc e]

       (tag? e)
       (let [[tag attrs? children]
             (let [[tag & attrs-and-children] e]
               (if (map? (first attrs-and-children))
                 [tag (first attrs-and-children) (vec (rest attrs-and-children))]
                 [tag nil (vec attrs-and-children)]))


             ;; Process child nodes: convert @mentions into links, etc..
             [mentions children]
             (reduce
              (fn [[mentions new-children] child]
                (cond
                  (string? child)
                  (let [[mentions processed-text]
                        ;; Mentions->links.
                        (mapv-acc
                         (fn [mentions msg-part]
                           (cond
                             ;; Not a mention.
                             (string? msg-part)
                             [mentions msg-part]

                             ;; Mention map.
                             (map? msg-part)
                             (let [{:keys [target original-text]} msg-part

                                   {:keys [ap-id] :as user}
                                   (or (mentions target)
                                       (account-name-to-link target))]
                               (if user
                                 ;; Return the original text back if the user couldn't be found.
                                 [(assoc mentions target user)
                                  [:a {:href ap-id} target]]
                                 [mentions original-text]))))
                         mentions
                         (split-by-mentions child))]
                    [mentions (into new-children processed-text)])

                  :else
                  [mentions (conj new-children child)]))
              [mentions []]
              children)

             just-tag (keyword (first (cstr/split (name tag) #"[.]|[#]")))]
         [(assoc acc :mentions mentions) ; Update the accumulator.
          ;; If no verifier or the verifier returns nil: remove the tag.
          (when-let [verifier (whitelist just-tag)]
            (when-let [[just-tag attrs] (verifier tag attrs?)]
              (let [children (->> children (remove nil?) vec)]
                (cond-> [(if ids-and-classes? tag just-tag)]
                  (not-empty attrs) (conj attrs)
                  (not-empty children) (into children)))))])

       :else
       (throw (ex-info "Invalid hiccough syntax" {:at e}))))
   initial-acc
   hiccough))
  ([hiccough whitelist ids-and-classes? account-name-to-link]
   (sanitize-hiccough hiccough whitelist ids-and-classes? account-name-to-link default-hiccough-accumulator)))
