(ns org.mushin.hiccough
  (:require [clojure.walk :as walk]
            [clojure.string :as str]
            [org.mushin.multimedia.svg :refer [set-attr! create-elem
                                               create-elem-ns set-attr-ns!
                                               add-child! create-text-node]])
  (:import [org.w3c.dom.svg SVGDocument]
           [org.w3c.dom Node]))

(defn- kebab-case->camel-case
  "Convert a keyword in kebab-case into a keyword with camelCase.

  # Arguments
    - `k`: The keyword to convert to camelCase.

  # Return value
  The keyword `k` with all kebab-case instances converted to camelCase."
  [k]
  (let [words (str/split (name k) #"-")]
    (->> (map str/capitalize (rest words))
         (apply str (first words))
         keyword)))


(defn- postwalk
  "A version of postwalk that does not recurse into maps (note that `f` will still be applied
  to the map itself, just not its key value pairs).

  # Arguments
    - `f`: A function applied to each element.
    - `form`: The form to process
  # Return value
  The result of `f` applied to the parent node of `form`."
  [f form]
  (walk/walk
   (if (map? form)
     identity
     (partial postwalk f)) f form))

(defn- tag?
  "Determine if `e` is a SVG tag in hiccough syntax.

  # Arguments
    - `e`: Object to determine if it is an SVG tag or not.

  # Return value
  `true` if `e` is a tag, `false` if not."
  [e]
  (and (vector? e) (keyword? (first e))))

(defn sanitize-hiccough
  [hiccough whitelist ids-and-classes?]
  (postwalk
   (fn [e]
     (cond
       (or (map? e)
           (keyword? e)
           (number? e)
           (boolean? e)
           (string? e))
       e

       (tag? e)
       (let [[tag attrs? children]
             (let [[tag & attrs-and-children] e]
               (if (map? (first attrs-and-children))
                 [tag (first attrs-and-children) (vec (rest attrs-and-children))]
                 [tag nil (vec attrs-and-children)]))
             just-tag (keyword (first (str/split (name tag) #"[.]|[#]")))]
         (when-let [verifiers (whitelist just-tag)]
           (let [attrs (into {}
                             (filter
                              (fn [[attr v]] (when-let [verifier (verifiers attr)]
                                            (verifier v))))
                             attrs?)
                 children (->> children (remove nil?) vec)]
             (cond-> [(if ids-and-classes? tag just-tag)]
               (not-empty attrs) (conj attrs)
               (not-empty children) (into children)))))

       :else
       (throw (ex-info "Invalid hiccough syntax" {:at e}))))
   hiccough))

(defn hiccough->svg!
  "Convert SVG hiccough DSL syntax into a SVGDocument. It places
  its document tags into the provided coument.

  `hiccough` is just `hiccup` syntax.

  # Arguments
    - `doc`: SVGDocument to place the tags into.
    - `hiccough`: DSL syntax.

  # Return value
  The SVGDocument."
  ^SVGDocument
  [^SVGDocument doc hiccough]
  (postwalk
   (fn [e]
     (cond
       (map? e)
       (walk/postwalk (fn [x] (if (map? x) (update-keys x kebab-case->camel-case) x))  e)

       (tag? e)
       ;; Create the tag and add children, attributes.
       (when-let [[tag attrs? children]
                  (let [[tag & attrs-and-children] e]
                    (when tag
                      (if (map? (first attrs-and-children))
                        [tag (first attrs-and-children) (vec (rest attrs-and-children))]
                        [tag nil (vec attrs-and-children)])))]

         (let [elem
               (let [tag-str (name tag)
                     tag-ns (namespace tag)
                     [tag-name & tag-parts] (str/split tag-str #"[.]|[#]")
                     [id classes] (if (str/includes? tag-str "#")
                                    [(first tag-parts) (str/join " " (rest tag-parts))]
                                    [nil (str/join " " tag-parts)])
                     elem (if (= tag :svg)
                            (.getDocumentElement doc)
                            (if tag-ns
                              (create-elem-ns doc tag-ns tag-name)
                              (create-elem doc tag-name)))]
                 ;; Destructure the class name into tag name, namespace, classes, id.
                 (when id
                   (set-attr! elem "id" id))
                 (when-not (str/blank? classes)
                   (set-attr! elem "class" classes))
                 elem)]

           (when attrs?
             ;; Add attributes to the tag.
             (doseq [[k v] attrs?]
               (let [tag-namespace (namespace k)
                     tag-name (name k)
                     v (str v)]
                 (if tag-namespace
                   (set-attr-ns! elem tag-namespace tag-name v)
                   (set-attr! elem tag-name v)))))

           (doseq [node children]
             ;; Add child nodes.
             (cond
               ;; Syntax sugar for href tags.
               (or (= tag :image)
                   (= tag :a)
                   (= tag :fe-image))
               (set-attr! elem "href" (str node))

               ;; Add element.
               (instance? Node node)
               (add-child! elem node)

               :else
               (add-child! elem (create-text-node doc (str node)))))
           elem))

       :else
       e))
   hiccough)
  doc)

