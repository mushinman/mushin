(ns org.mushin.hiccough
  (:require [org.mushin.acc :refer [postwalk-noassoc mapv-acc]]
            [org.mushin.multimedia.svg :as svg]
            [org.mushin.geom :as geom]
            [org.mushin.db.resource-meta :as res-meta]
            [clojure.string :as cstr]
            [ring.util.response :as res])
  (:import [java.util.regex Matcher]
           [java.net URI]))

(def ^:private panel-width 1080.0)

(def ^:private panel-height 1920.0)


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

(defn- non-negative?
  "Returns true if `n` is a number type, is non-negative (>= 0), and is not infinite."
  [n]
  (and (number? n)
       (or (pos? n) (zero? n))
       (not (Double/isInfinite (double n)))))

(defn- valid-uri?
  "Returns true if `uri` is a valid URI that is not a dangerous scheme
  (javascript:, vbscript:)."
  [uri]
  (try
    (and
     (not (re-matches #"(?i)^\s*(javascript|vbscript)\:.*" uri))
     (URI. uri))
    (catch Throwable _
      false)))

(defn- lookup-resource
  "Lookup a resource according to its ID, using a resource cache.

  # Arguments
  - `get-resource`: A function that takes in a resource ID and returns a URI to that resource.
  - `resource-map-cache`: Cached results for `get-resource`. Must be assocable.
  - `resource-id`: The resource ID to fetch a URI to.

  # Return value
  A 2-member tuple with a new cache in the first position and the result URI in the second."
  [get-resource resource-map-cache resource-id]
  (if-let [res (resource-map-cache resource-id)]
    [resource-map-cache res]
    (let [res (get-resource resource-id)]
      [(assoc resource-map-cache resource-id res) res])))

(defn- validate-comic-tag
  "Validate a tag and an attribute.

  # Arguments
  - `get-resource`: A function that takes in a resource ID and returns a URI to that resource.
  - `resource-map-cache`: Cached results for `get-resource`. Must be assocable.
  - `tag`: The tag to validate.
  - `attrs`: An assocable of attributes for the tag.

  # Return value
  A 2-member tuple with a new cache in the first position and the result in the second.

  The validated tag will be a vector with a tag and maybe an attribute assocable. Or `nil`
  if the tag was rejected.
  "
  [resource-map-cache get-resource tag attrs]
  (case tag
    ;; Just verify that href is a valid URI (and exists).
    :a [resource-map-cache
        (when-let [href (:href attrs)]
          (when (valid-uri? href)
            [:a {:href href :rel "noopener noreferrer"}]))]

    ;; Validate resource.
    :image
    (let [{:keys [src width height x y]}
          attrs

          [resource-map-cache location]
          (lookup-resource get-resource resource-map-cache src)]
      [resource-map-cache
       (when (and src (non-negative? width) (non-negative? height) location
                  (non-negative? x) (non-negative? y))
         [tag {:src location
               :width width 
               :height height 
               :x x 
               :y y}])])

    ;; Text.
    (:p :h1 :h2 :h3 :h4 :h5 :h6 :code :em :strong)
    (let [{:keys [width height x y]} attrs]
      [resource-map-cache 
       (when (and (non-negative? width) (non-negative? height)
                  (non-negative? x) (non-negative? y))
         [tag {:width width 
               :height height 
               :x x 
               :y y}])])

    :rect
    (let [{:keys [width height x y fill stroke stroke-width]} attrs]
      [resource-map-cache 
       (when (and (non-negative? width) (non-negative? height)
                  (non-negative? x) (non-negative? y) fill stroke
                  (non-negative? stroke-width))
         [tag {:width width 
               :height height 
               :x x 
               :y y
               :fill fill
               :stroke stroke
               :stroke-width stroke-width}])])


    ;; Ignore attributes.
    (:comic :panel)
    [resource-map-cache [tag]]

    ;; Default: reject unknown tags.
    [resource-map-cache nil]))

(defn- validate-meme-tag
  "Validate a tag and an attribute.

  # Arguments
  - `tag`: The tag to validate.
  - `attrs`: An assocable of attributes for the tag.

  # Return value
  The validated tag will be a vector with a tag and maybe an attribute assocable. Or `nil`
  if the tag was rejected.
  "
  [tag attrs]
  (case tag
    ;; Just verify that href is a valid URI (and exists).
    :a 
    (when-let [href (:href attrs)]
      (when (valid-uri? href)
        [:a {:href href :rel "noopener noreferrer"}]))

    ;; Text.
    (:p :h1 :h2 :h3 :h4 :h5 :h6 :code :em :strong)
    (let [{:keys [x y]} attrs]
      (when (and (non-negative? x) (non-negative? y))
        [tag {:x x 
              :y y}]))

    ;; Default: reject unknown tags.
    nil))

(defn- validate-microblog-tag
  "Validate a tag and an attribute.

  # Arguments
  - `get-resource`: A function that takes in a resource ID and returns a URI to that resource.
  - `resource-map-cache`: Cached results for `get-resource`. Must be assocable.
  - `tag`: The tag to validate.
  - `attrs`: An assocable of attributes for the tag.

  # Return value
  A 2-member tuple with a new cache in the first position and the result in the second.

  The validated tag will be a vector with a tag and maybe an attribute assocable. Or `nil`
  if the tag was rejected.
  "
  [resource-map-cache get-resource tag attrs]
  (case tag
    ;; Just verify that href is a valid URI (and exists).
    :a [resource-map-cache
        (when-let [href (:href attrs)]
          (when (valid-uri? href)
            [:a {:href href :rel "noopener noreferrer"}]))]

    ;; Validate resource.
    :image
    (let [{:keys [src]}
          attrs

          [resource-map-cache location]
          (lookup-resource get-resource resource-map-cache src)]
      [resource-map-cache
       (when (and src location)
         [tag {:src location}])])

    ;; Ignore attributes.
    (:p :h1 :h2 :h3 :h4 :h5 :h6 :code :em :strong :div)
    [resource-map-cache [tag]]

    ;; Default: reject unknown tags.
    [resource-map-cache nil]))

(defn- tag?
  "Determine if `e` is a SVG tag in hiccough syntax.

  # Arguments
    - `e`: Object to determine if it is an SVG tag or not.

  # Return value
  `true` if `e` is a tag, `false` if not."
  [e]
  (and (vector? e) (keyword? (first e))))


(defn sanitize-comic-hiccough
  "Sanitize hiccough syntax for comics.

  # Arguments
  - `hiccough`: Comic hiccough syntax to sanitize.
  - `ids-and-classes?`: If true, keep IDs and classes in the hiccough. If false, remove them.
  - `account-name-to-link`: A function that takes in a nickname, which could be fully qualifed,
  and returns a URI to the account referenced by the nickname, or nil if the account doesn't exist.
  - `res-id-to-res`: A function that takes in a resource ID and returns a link to the resource object,
  or nil if the resource object does not exist.

  # Return value
  A 2-member tuple containing metadata in the first position and the result in the second.

  The metadata has the following format:
| Key                | Type                 | Meaning                                                                   |
|:-------------------|:---------------------|:--------------------------------------------------------------------------|
| `:mentions`        | Map of string -> URI | Map of (maybe fully qualified) nicknames to a user struct                 |
| `:resource-cache`  | Map of string -> URI | Map of resource IDs to a URI to that resource                             |
| `:panel-transform` | AffineTransform      | A transform to the bottom to the top of the final panel                   |

  The result is itself a 2-member tuple. The sanitized hiccough is in the first position, and a SVG document of the comic
  is in the second position.
  "
  [hiccough ids-and-classes? account-name-to-link res-id-to-res]
  (let [doc (svg/create-doc)
        [acc [hiccup _]]
        (postwalk-noassoc
         (fn [{:keys [mentions panel-transform resource-cache] :as acc} e]
           (cond
             ;; Do not process yet.
             (or (map? e)
                 (keyword? e)
                 (string? e))
             [acc e]

             ;; Convert to a tuple of the e and a text node.
             (or (number? e)
                 (boolean? e))
             [acc [e (svg/create-text-node doc (str e))]]
                 
                 

             ;; Since this function produces both hiccup and a SVG document result when we
             ;; return a tuple of processed hiccup and a SVG node, e.g. [:p "Hello world"]
             ;; becomes [[:p "Hello world"] Text{"Hello world"}].
             (tag? e)
             (let [[tag attrs? children]
                   (let [[tag & attrs-and-children] e]
                     (if (map? (first attrs-and-children))
                       [tag (first attrs-and-children) (vec (rest attrs-and-children))]
                       [tag nil (vec attrs-and-children)]))

                   ;; Process child nodes: convert @mentions into links, etc..
                   ;; If not a mention the children will be tuples of the text and a SVG node.
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
                                   (string? msg-part)
                                   [mentions [msg-part (svg/create-text-node doc msg-part)]]

                                   ;; Mention map.
                                   (map? msg-part)
                                   (let [{:keys [target original-text]} msg-part

                                         {:keys [ap-id] :as user}
                                         (or (mentions target)
                                             (account-name-to-link target))]
                                     (if user
                                       [(assoc mentions target user)
                                        [[:a {:href ap-id :rel "noopener noreferrer"} target]
                                        (svg/create-a-tag doc ap-id target)]]
                                       ;; Return the original text back if the user couldn't be found.
                                       [mentions original-text]))))
                               mentions
                               (split-by-mentions child))]
                          [mentions (into new-children processed-text)])

                        :else
                        [mentions (conj new-children child)]))
                    [mentions []]
                    children)

                   just-tag (keyword (first (cstr/split (name tag) #"[.]|[#]")))

                   [resource-cache verified-tag]
                   (validate-comic-tag resource-cache res-id-to-res just-tag attrs?)]
               (if verified-tag
                 (let [[just-tag attrs] verified-tag
                       [elem panel-transform] ; SVG node and accumulator changes.
                       (case tag
                         :comic
                         [(let [svg-elem (svg/get-root doc)]
                            ;; The aspect ratio is 9:16 for each panel, so the height is
                            ;; panel-height * number of panels.
                            (svg/set-attr! svg-elem "viewBox" (svg/viewbox-str 0 0 panel-width
                                                                               (* panel-height (count children))))
                            (svg/set-attr! svg-elem "preserveAspectRatio" "xMidYMid meet")
                            svg-elem)
                          panel-transform]

                         :panel
                         [(let [g (svg/create-elem doc "g")]
                            ;; Translate the panel down the comic to its proper place.
                            (svg/set-attr! g "transform" (svg/affine-transform->transform-str panel-transform))
                            g)
                          ;; Move the panel transform down one panel.
                          (geom/translate-transform panel-transform 0.0 panel-height)]

                         :p
                         [(svg/create-elem doc "text") panel-transform]

                         ;; Any other tag.
                         [(svg/create-elem doc just-tag)
                          panel-transform])


                       hiccup-children
                       (mapv (fn [[hiccup-child svg-child]]
                               (svg/add-child! elem svg-child)
                               hiccup-child)
                             (filter some? children))]

                   (when attrs
                     ;; Add attributes to the tag.
                     (doseq [[k v] attrs]
                       (let [tag-namespace (namespace k)
                             tag-name (name k)
                             v (str v)]
                         (if tag-namespace
                           (svg/set-attr-ns! elem tag-namespace tag-name v)
                           (svg/set-attr! elem tag-name v)))))


                   [;; Update the accumulator.
                    (assoc acc
                           :mentions mentions
                           :panel-transform panel-transform
                           :resource-cache resource-cache)
                    ;; Hiccup and SVG tag.
                    [(cond-> [(if ids-and-classes? tag just-tag)]
                       (not-empty attrs) (conj attrs)
                       (not-empty hiccup-children) (into hiccup-children))
                     elem]])
                 [acc nil]))

             :else
             (throw (ex-info "Invalid hiccough syntax" {:at e}))))
         {:mentions {}
          :panel-transform (geom/affine-transform) ; For moving comic panels down the SVG.
          :resource-cache {} ; Cache of used resources. ID -> link.
          }
         hiccough)]
    [acc [hiccup doc]]))

(defn sanitize-meme-hiccough
  "Sanitize hiccough syntax for comics.

  # Arguments
  - `hiccough`: Comic hiccough syntax to sanitize.
  - `ids-and-classes?`: If true, keep IDs and classes in the hiccough. If false, remove them.
  - `account-name-to-link`: A function that takes in a nickname, which could be fully qualifed,
  and returns a URI to the account referenced by the nickname, or nil if the account doesn't exist.
  - `res-id-to-res`: A function that takes in a resource ID and returns a link to the resource object,
  or nil if the resource object does not exist.

  # Return value
  A 2-member tuple containing metadata in the first position and the result in the second.

  The metadata has the following format:
| Key                | Type                 | Meaning                                                                   |
|:-------------------|:---------------------|:--------------------------------------------------------------------------|
| `:mentions`        | Map of string -> URI | Map of (maybe fully qualified) nicknames to a user struct                 |

  The result is itself a 2-member tuple. The sanitized hiccough is in the first position, and a SVG document of the comic
  is in the second position.
  "
  [hiccough ids-and-classes? account-name-to-link
   image-pixel-width image-pixel-height caption-pixel-height]
  (let [doc (svg/create-doc)
        [acc [hiccup _]]
        (postwalk-noassoc
         (fn [{:keys [mentions] :as acc} e]
           (cond
             ;; Do not process yet.
             (or (map? e)
                 (keyword? e)
                 (string? e))
             [acc e]

             ;; Convert to a tuple of the e and a text node.
             (or (number? e)
                 (boolean? e))
             [acc [e (svg/create-text-node doc (str e))]]
                 
                 

             ;; Since this function produces both hiccup and a SVG document result when we
             ;; return a tuple of processed hiccup and a SVG node, e.g. [:p "Hello world"]
             ;; becomes [[:p "Hello world"] Text{"Hello world"}].
             (tag? e)
             (let [[tag attrs? children]
                   (let [[tag & attrs-and-children] e]
                     (if (map? (first attrs-and-children))
                       [tag (first attrs-and-children) (vec (rest attrs-and-children))]
                       [tag nil (vec attrs-and-children)]))

                   ;; Process child nodes: convert @mentions into links, etc..
                   ;; If not a mention the children will be tuples of the text and a SVG node.
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
                                   (string? msg-part)
                                   [mentions [msg-part (svg/create-text-node doc msg-part)]]

                                   ;; Mention map.
                                   (map? msg-part)
                                   (let [{:keys [target original-text]} msg-part

                                         {:keys [ap-id] :as user}
                                         (or (mentions target)
                                             (account-name-to-link target))]
                                     (if user
                                       [(assoc mentions target user)
                                        [[:a {:href ap-id :rel "noopener noreferrer"} target]
                                        (svg/create-a-tag doc ap-id target)]]
                                       ;; Return the original text back if the user couldn't be found.
                                       [mentions original-text]))))
                               mentions
                               (split-by-mentions child))]
                          [mentions (into new-children processed-text)])

                        :else
                        [mentions (conj new-children child)]))
                    [mentions []]
                    children)

                   just-tag (keyword (first (cstr/split (name tag) #"[.]|[#]")))

                   verified-tag
                   (validate-meme-tag just-tag attrs?)]
               (if verified-tag
                 (let [[just-tag attrs] verified-tag
                       elem ; SVG node and accumulator changes.
                       (case tag
                         :meme
                         (let [svg-elem (svg/get-root doc)
                                virtual-width 1000
                                img-virtual-height (* virtual-width (/ (double image-pixel-height) (double image-pixel-width)))
                                caption-virtual-height (* img-virtual-height (/ (double caption-pixel-height) (double image-pixel-height)))
                                total-virtual-height (+ caption-virtual-height img-virtual-height)]
                            (svg/set-attr! svg-elem "width" "100%")
                            ;; We preserve the aspect ratio of the original image.
                            (svg/set-attr!
                             svg-elem
                             "viewBox"
                             (svg/viewbox-str 0 0 virtual-width total-virtual-height))
                            (svg/set-attr! svg-elem "preserveAspectRatio" "xMidYMid meet")
                            svg-elem)

                         :p
                         (svg/create-elem doc "text")

                         ;; Any other tag.
                         (svg/create-elem doc just-tag))


                       _
                       (mapv (fn [[hiccup-child svg-child]]
                               (svg/add-child! elem svg-child)
                               hiccup-child)
                             (filter some? children))]

                   (when attrs
                     ;; Add attributes to the tag.
                     (doseq [[k v] attrs]
                       (let [tag-namespace (namespace k)
                             tag-name (name k)
                             v (str v)]
                         (if tag-namespace
                           (svg/set-attr-ns! elem tag-namespace tag-name v)
                           (svg/set-attr! elem tag-name v)))))


                   [;; Update the accumulator.
                    (assoc acc :mentions mentions)
                    ;; Hiccup and SVG tag.
                    [(cond-> [(if ids-and-classes? tag just-tag)]
                       (not-empty attrs) (conj attrs))
                     elem]])
                 [acc nil]))

             :else
             (throw (ex-info "Invalid hiccough syntax" {:at e}))))
         {:mentions {}}
         hiccough)]
    [acc doc]))

(defn sanitize-microblog-hiccough
  "Sanitize hiccough syntax for microblogs.

  # Arguments
  - `hiccough`: Microblog hiccough syntax to sanitize.
  - `ids-and-classes?`: If true, keep IDs and classes in the hiccough. If false, remove them.
  - `account-name-to-link`: A function that takes in a nickname, which could be fully qualifed,
  and returns a URI to the account referenced by the nickname, or nil if the account doesn't exist.
  - `res-id-to-res`: A function that takes in a resource ID and returns a link to the resource object,
  or nil if the resource object does not exist.

  # Return value
  A 2-member tuple containing metadata in the first position and the result in the second.

  The metadata has the following format:
| Key               | Type                 | Meaning                                                                    |
|:------------------|:---------------------|:---------------------------------------------------------------------------|
| `:mentions`        | Map of string -> URI | Map of (maybe fully qualified) nicknames to a user struct               |
| `:resource-cache` | Map of string -> URI | Map of resource IDs to a URI to that resource                              |

  The result is a single-element vector containing the sanitized hiccough.
  "
  [hiccough ids-and-classes? account-name-to-link res-id-to-res]
  (let [[acc [hiccup]]
        (postwalk-noassoc
         (fn [{:keys [mentions resource-cache] :as acc} e]
           (cond
             ;; Do not process yet.
             (or (map? e)
                 (keyword? e)
                 (string? e))
             [acc e]

             ;; Convert to a tuple of the e and a text node.
             (or (number? e)
                 (boolean? e))
             [acc [e]]
                 
                 

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
                                   (string? msg-part)
                                   [mentions [msg-part]]

                                   ;; Mention map.
                                   (map? msg-part)
                                   (let [{:keys [target original-text]} msg-part

                                         {:keys [ap-id] :as user}
                                         (or (mentions target)
                                             (account-name-to-link target))]
                                     (if user
                                       [(assoc mentions target user)
                                        [[:a {:href ap-id :rel "noopener noreferrer"} target]]]
                                       ;; Return the original text back if the user couldn't be found.
                                       [mentions original-text]))))
                               mentions
                               (split-by-mentions child))]
                          [mentions (into new-children processed-text)])

                        :else
                        [mentions (conj new-children child)]))
                    [mentions []]
                    children)

                   just-tag (keyword (first (cstr/split (name tag) #"[.]|[#]")))

                   [resource-cache verified-tag]
                   (validate-microblog-tag resource-cache res-id-to-res just-tag attrs?)]
               (if verified-tag
                 (let [[just-tag attrs] verified-tag
                       hiccup-children
                       (for [[hiccup-child] (filter some? children)]
                         ;; Nil children are ignored.
                           hiccup-child)]

                   [;; Update the accumulator.
                    (assoc acc
                           :mentions mentions
                           :resource-cache resource-cache)
                    [(cond-> [(if ids-and-classes? tag just-tag)]
                       (not-empty attrs) (conj attrs)
                       (not-empty hiccup-children) (into hiccup-children))]])
                 [acc nil]))

             :else
             (throw (ex-info "Invalid hiccough syntax" {:at e}))))
         {:mentions {}
          :resource-cache {}}
         hiccough)]
    [acc [hiccup]]))


