(ns org.mushin.multimedia.svg
  (:require [clojure.java.io :as io]
            [org.mushin.multimedia.buffered-image-transcoder :as bit]
            [clojure.walk :as walk]
            [clojure.string :as str]
            [org.mushin.files :as files])
  (:import [io.sf.carte.echosvg.transcoder TranscoderInput SVGAbstractTranscoder]
           [io.sf.carte.echosvg.anim.dom SVGDOMImplementation]
           [io.sf.carte.echosvg.dom GenericAttr]
           [javax.xml.transform TransformerFactory OutputKeys]
           [io.sf.carte.echosvg.anim.dom SAXSVGDocumentFactory]
           [io.sf.carte.echosvg.svggen SVGGraphics2D]
           [org.w3c.dom Element Text Node]
           [org.w3c.dom.svg SVGDocument SVGFitToViewBox SVGElement]
           [javax.xml.transform.stream StreamResult]
           [javax.xml.transform.dom DOMSource]
           [java.awt.image BufferedImage]
           [java.io StringWriter OutputStreamWriter]
           [java.nio.file Path]))

(def ^String svg-ns SVGDOMImplementation/SVG_NAMESPACE_URI)

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

(defn create-doc
  "Create a SVGDocument.

  # Return value
  A new SVGDocument."
  ^SVGDocument
  []
  (.createDocument (SVGDOMImplementation/getDOMImplementation) svg-ns "svg" nil))

(defn open-svg-doc
  ^SVGDocument
  [^String svg-path]
  (let [factory (SAXSVGDocumentFactory.)]
    (.createDocument factory svg-path)))

(defn create-tag
  "Create a SVGElement.

  # Arguments
    - `doc`: The SVGDocument to create the element with.
    - `tag-name`: The name of the element.

  # Return value
  A new Element."
  ^Element
  [^SVGDocument doc ^String tag-name]
  (.createElementNS doc svg-ns tag-name))

(defn get-root
  "Get the root <svg> tag of a document.

  # Arguments
    - `doc`: The document to get the root element of.

  # Return value
  The root <svg> tag of `doc`."
  ^SVGElement
  [^SVGDocument doc]
  (.getDocumentElement doc))

(defn create-attr
  "Create an attribute.

  # Arguments
    - `doc`: The document to create the element for.
    - `name`: The name of the attribute to create.
    - `value` (optional): The value of the attribute.

  # Return value
  A new attribute."
  ^GenericAttr
  ([^SVGDocument doc ^String name]
   (GenericAttr. name doc))
  ([^SVGDocument doc ^String name ^String value]
   (doto (GenericAttr. name doc)
     (.setValue value))))

(defn add-attr-ns!
  "Create an attribute with a namespace.

  # Arguments
    - `doc`: The document to create the element for.
    - `ns`: The namespace of the attribute.
    - `name`: The name of the attribute to create.
    - `value` (optional): The value of the attribute.

  # Return value
  A new attribute."
  ^GenericAttr
  ([^SVGDocument doc ^SVGElement element ^String ns ^String name ^String value]
   (.setAttributeNS element ns (create-attr doc name value )))
  ([^SVGDocument doc ^SVGElement element ^String ns ^String name]
   (.setAttribute element ns (create-attr doc name))))

(defn add-attr!
  "Create an attribute on an element, modifying the element.

  # Arguments
    - `doc`: The document to create the element for.
    - `element`: The element of the to add the attribute to.
    - `name`: The name of the attribute to create.
    - `value` (optional): The value of the attribute.

  # Return value
  A new attribute."
  ^GenericAttr
  ([^SVGDocument doc ^SVGElement element ^String name ^String value]
   (add-attr-ns! doc element svg-ns name value))
  ([^SVGDocument doc ^SVGElement element ^String name]
   (add-attr-ns! doc element svg-ns name)))

(defn set-attr-ns!
  "Create an attribute on an element with a namespace, modifying the element.

  # Arguments
    - `doc`: The document to create the element for.
    - `element`: The element of the to add the attribute to.
    - `ns`: The namespace of the attribute.
    - `name`: The name of the attribute to create.
    - `value` (optional): The value of the attribute.

  # Return value
  A new attribute."
  ^Element
  [^SVGElement element ^String ns ^String name ^String value]
  (.setAttributeNS element ns name value)
  element)

(defn set-attr!
  "Set an attribute on an element with a namespace, modifying the element.

  # Arguments
    - `element`: The element of the to add the attribute to.
    - `ns`: The namespace of the attribute.
    - `name`: The name of the attribute to create.
    - `value` (optional): The value of the attribute.

  # Return value
  The input element."
  ^Element
  [^SVGElement element ^String name ^String value]
  (.setAttribute element name value)
  element)

(defn create-elem-ns
  "Create a new element on a doc.

  # Arguments
    - `doc`: The SVGDocument to create the element for.
    - `ns`: The namespace of the element.
    - `elem-name`: The name of the element type to create.

  # Return value
  The new element."
  ^Element
  [^SVGDocument doc ^String ns ^String elem-name]
  (.createElementNS doc ns elem-name))

(defn create-elem
  "Create a new element on a doc.

  # Arguments
    - `doc`: The SVGDocument to create the element for.
    - `elem-name`: The name of the element type to create.

  # Return value
  The new element."
  ^Element
  [^SVGDocument doc ^String elem-name]
  (.createElement doc elem-name))

(defn add-child!
  "Add a element to another element as a child, alterning the element.

  # Arguments
    - `parent`: The SVGDocument to create the element for.
    - `child`: The name of the element type to create.

  # Return value
  The parent element."
  ^Element
  [^Element parent ^Element child]
  (.appendChild parent child)
  parent)

(defn create-text-node
  "Create a text node for a document.

  # Arguments
    - `doc`: The SVGDocument to create the element for.
    - `text`: The value of the text node.

  # Return value
  The created Text element."
  ^Text
  [^SVGDocument doc ^String text]
  (.createTextNode doc text))

(defn add-tag!
  "Add an elemen to a document.  This call modifies the document.

  # Arguments
    - `doc`: The SVGDocment to add the element to.
    - `tag-name`: The name of tag to add.

  # Return value
  The Element added to the document."
  ^Element
  [^SVGDocument doc ^String tag-name ]
  (let [tag (create-tag doc tag-name)]
    (.appendChild (get-root doc) tag)
    tag))

(defn doc->string
  "Convert a SVGDocument to a string.

  # Arguments
    - `doc`: SVGDocument to convert into a string.

  # Return value
  `doc` converting into a string."
  ^String
  [^SVGDocument doc]
  (let [tf (TransformerFactory/newInstance)
        tr (.newTransformer tf)
        sw (StringWriter.)]
    (.setOutputProperty tr OutputKeys/OMIT_XML_DECLARATION "no")
    (.setOutputProperty tr OutputKeys/METHOD "xml")
    (.setOutputProperty tr OutputKeys/ENCODING "UTF-8")
    (.setOutputProperty tr OutputKeys/INDENT "yes")
    (.transform tr (DOMSource. doc) (StreamResult. sw))
    (.toString sw)))

(defn- tag?
  "Determine if `e` is a SVG tag in hiccough syntax.

  # Arguments
    - `e`: Object to determine if it is an SVG tag or not.

  # Return value
  `true` if `e` is a tag, `false` if not."
  [e]
  (and (vector? e) (keyword? (first e))))

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

(defn hiccough->svg
  "Convert SVG hiccough DSL syntax into a SVGDocument.

  `hiccough` is just `hiccup` syntax.

  # Arguments
    - `hiccough`: DSL tree.

  # Return value
  The SVGDocument.
  "
  ^SVGDocument
  [dsl-map]
  (hiccough->svg! (create-doc) dsl-map))

(defn write-svgdoc-to-file!
  [^SVGDocument doc file]
  (with-open [osw (-> (io/output-stream (if (instance? Path file)
                                          (str file)
                                          file))
                      (OutputStreamWriter. files/charset-utf8))]
    (.stream (SVGGraphics2D. doc) (.getDocumentElement doc) osw))
  file)

(defn get-viewbox-of
  [element]
  (when (instance? SVGFitToViewBox element)
    (let [rect (.. element (getViewBox) (getBaseVal))]
      {:x (.getX rect) :y (.getY rect) :width (.getWidth rect) :height (.getHeight rect)})))

(defn render-document
  ^BufferedImage
  [doc-file ^double width-px ^double height-px]
  (let [^SVGDocument document (if (instance? SVGDocument doc-file)
                                doc-file
                                (open-svg-doc doc-file))]
    @(doto (bit/buffered-image-transcoder files/tmp-dir)
       (.addTranscodingHint SVGAbstractTranscoder/KEY_WIDTH (float width-px))
       (.addTranscodingHint SVGAbstractTranscoder/KEY_HEIGHT (float height-px))
       (.addTranscodingHint SVGAbstractTranscoder/KEY_RESOLUTION_DPI (float 96))
       (.addTranscodingHint SVGAbstractTranscoder/KEY_EXECUTE_ONLOAD Boolean/TRUE)
       (.addTranscodingHint SVGAbstractTranscoder/KEY_ALLOWED_SCRIPT_TYPES "")
       (.addTranscodingHint SVGAbstractTranscoder/KEY_CONSTRAIN_SCRIPT_ORIGIN Boolean/TRUE)
       (.addTranscodingHint SVGAbstractTranscoder/KEY_ALLOW_EXTERNAL_RESOURCES Boolean/TRUE)
       (.transcode (TranscoderInput. document) nil))))
