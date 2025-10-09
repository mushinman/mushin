(ns org.mushin.multimedia.svg
  (:require [clojure.java.io :as io]
            [org.mushin.multimedia.buffered-image-transcoder :as bit]
            [org.mushin.files :as files])
  (:import [io.sf.carte.echosvg.transcoder TranscoderInput SVGAbstractTranscoder]
           [io.sf.carte.echosvg.anim.dom SVGDOMImplementation]
           [io.sf.carte.echosvg.dom GenericAttr]
           [javax.xml.transform TransformerFactory OutputKeys]
           [io.sf.carte.echosvg.anim.dom SAXSVGDocumentFactory]
           [io.sf.carte.echosvg.svggen SVGGraphics2D]
           [org.w3c.dom.svg SVGDocument SVGFitToViewBox SVGElement]
           [javax.xml.transform.stream StreamResult]
           [javax.xml.transform.dom DOMSource]
           [java.awt.image BufferedImage]
           [java.io StringWriter OutputStreamWriter]
           [java.nio.file Path]))


(def mushin-ns
  "XML namespace used for custom mushin extensions."
  "urn:mushin")

(def ^String svg-ns SVGDOMImplementation/SVG_NAMESPACE_URI)

(defn get-caption-pixel-height
  [virtual-height content-virtual-height content-pixel-height]
  (* (/ (- virtual-height content-virtual-height) content-virtual-height) content-pixel-height))


(defn get-content-virtual-height
  [content-virtual-width content-pixel-width content-pixel-height]
  (* content-virtual-width (/ content-pixel-height content-pixel-width)))

(defn open-svg-doc
  ^SVGDocument
  [^String svg-path]
  (let [factory (SAXSVGDocumentFactory.)]
    (.createDocument factory svg-path)))

(defn create-tag
  ^SVGElement
  [^SVGDocument doc ^String tag-name]
  (.createElementNS doc svg-ns tag-name))

(defn get-root
  ^SVGElement
  [^SVGDocument doc]
  (.getDocumentElement doc))

(defn create-attr
  ^GenericAttr
  ([^String name ^SVGDocument doc]
   (GenericAttr. name doc))
  ([^String name ^String value ^SVGDocument doc]
   (doto (GenericAttr. name doc)
     (.setValue value))))

(defn add-attr-ns!
  ^GenericAttr
  ([^SVGDocument doc ^SVGElement element ^String ns ^String name ^String value]
   (.setAttributeNS element ns (create-attr name value doc)))
  ([^SVGDocument doc ^SVGElement element ^String ns ^String name]
   (.setAttribute element ns (create-attr name doc))))

(defn add-attr!
  ^GenericAttr
  ([^SVGDocument doc ^SVGElement element ^String name ^String value]
   (add-attr-ns! doc element svg-ns name value))
  ([^SVGDocument doc ^SVGElement element ^String name]
   (add-attr-ns! doc element svg-ns name)))

(defn set-attr-ns!
  [^SVGElement element ^String ns ^String name ^String value]
  (.setAttribute element ns name value))

(defn set-attr!
  [^SVGElement element ^String name ^String value]
  (set-attr-ns! element svg-ns name value))

(defn create-elem-ns
  [^SVGDocument doc ^String ns ^String elem-name]
  (.createElementNS doc ns elem-name))

(defn create-elem
  [^SVGDocument doc ^String elem-name]
  (create-elem-ns doc svg-ns elem-name))

(defn add-tag!
  ^SVGElement
  ([^SVGDocument doc ^String tag-name attributes]
   (let [tag (create-tag doc tag-name)]
     (doseq [[^String k ^String v] attributes]
       (set-attr! tag k v))
     (.appendChild (get-root doc) tag)
     tag))
  ([^SVGDocument doc ^String tag-name]
   (add-tag! doc tag-name {})))

(defn doc->string
  ^String [^SVGDocument doc]
  (let [tf (TransformerFactory/newInstance)
        tr (.newTransformer tf)
        sw (StringWriter.)]
    (.setOutputProperty tr OutputKeys/OMIT_XML_DECLARATION "no")
    (.setOutputProperty tr OutputKeys/METHOD "xml")
    (.setOutputProperty tr OutputKeys/ENCODING "UTF-8")
    (.setOutputProperty tr OutputKeys/INDENT "yes")
    (.transform tr (DOMSource. doc) (StreamResult. sw))
    (.toString sw)))


(defn- parse-dsl
  [^SVGDocument doc ^SVGElement elem {:keys [attrs children]}]
  (doseq [[child-elem attrs-and-children]
          (map (fn [[elem-name attrs-and-children]]
                 [(create-elem-ns doc
                                  (or (namespace elem-name) svg-ns)
                                  (name elem-name))
                  attrs-and-children])
               children)]

    (recur doc child-elem attrs-and-children)))

(defn dsl-svg
  [dsl-map]
  (let [doc (.createDocument (SVGDOMImplementation/getDOMImplementation) svg-ns "svg" nil)]
    (parse-dsl doc (.getDocumentElement doc) dsl-map)))

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
