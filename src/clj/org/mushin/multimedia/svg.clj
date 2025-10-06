(ns org.mushin.multimedia.svg
  (:require [clojure.java.io :as io]
            [clojure.math :as math]
            [clojure.string :as str]
            [org.mushin.codecs :as b64]
            [org.mushin.files :as files]
            [java-time.api :as time])
  (:import [io.sf.carte.echosvg.transcoder TranscoderInput TranscoderException TranscoderOutput SVGAbstractTranscoder]
           [io.sf.carte.echosvg.transcoder.image PNGTranscoder]
           [io.sf.carte.echosvg.anim.dom SVGDOMImplementation]
           [javax.xml.transform TransformerFactory OutputKeys]
           [javax.imageio ImageIO ImageWriter ImageReader IIOImage ImageTypeSpecifier]
           [io.sf.carte.echosvg.anim.dom SAXSVGDocumentFactory]
           [io.sf.carte.echosvg.bridge UserAgentAdapter DocumentLoader BridgeContext GVTBuilder ExternalResourceSecurity ScriptSecurity]
           [io.sf.carte.echosvg.svggen SVGGraphics2D]
           [io.sf.carte.echosvg.gvt GraphicsNode]
           [io.sf.carte.echosvg.util ParsedURL]
           [io.sf.carte.echosvg.dom.util DOMUtilities]
           [org.w3c.dom.svg SVGDocument SVGFitToViewBox SVGElement]
           [javax.imageio ImageIO ImageWriter]
           [java.io ByteArrayInputStream OutputStream]
           [javax.xml.transform.stream StreamResult]
           [javax.xml.transform.dom DOMSource]
           [java.awt.geom AffineTransform Rectangle2D]
           [java.awt.image BufferedImage RenderedImage]
           [java.io StringWriter OutputStreamWriter]
           [java.nio.file Path]
           [org.mushin.multimedia BufferedImageTranscoder]))


(def mushin-ns
  "XML namespace used for custom mushin extensions."
  "urn:mushin")

(def ^String svg-ns SVGDOMImplementation/SVG_NAMESPACE_URI)

(defn get-caption-pixel-height
  [virtual-height content-virtual-height content-pixel-height]
  (* (/ (- virtual-height content-virtual-height) content-virtual-height) content-pixel-height))

(defn file-root-ua [^String root-path]
  (let [root (-> (files/path root-path)
                 (files/to-absolute-path)
                 (files/normalize))]
    (proxy [UserAgentAdapter] []
      (getExternalResourceSecurity
        [^ParsedURL res ^ParsedURL doc]
        (reify ExternalResourceSecurity
          (checkLoadExternalResource [_]
            (println "file-root-ua")
            (when (not= "file" (.getProtocol res))
              (throw (SecurityException. "Only file protocol is allowed")))
            (let [p (-> (files/path (.getPath res))
                        (files/to-absolute-path)
                        (files/normalize))]
              (when-not (.startsWith p root)
                (throw (SecurityException. (str "Blocked outside the root path " root))))))))
      (getScriptSecurity
        [^String t ^ParsedURL s ^ParsedURL d]
        (reify ScriptSecurity
          (checkLoadScript [_]
            (throw (SecurityException. "Scripts are disabled"))))))))

(defmacro transcoder-with-file-root
  [root transcoder-type]
  `(proxy [~transcoder-type] []
      (createUserAgent [] (file-root-ua ~root))))

(defn get-content-virtual-height
  [content-virtual-width content-pixel-width content-pixel-height]
  (* content-virtual-width (/ content-pixel-height content-pixel-width)))

(defn open-svg-doc
  ^SVGDocument
  [^String svg-path]
  (let [factory (SAXSVGDocumentFactory.)]
    (.createDocument factory svg-path)))

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
    (.getImage
     (doto (transcoder-with-file-root files/tmp-dir BufferedImageTranscoder)
       (.addTranscodingHint SVGAbstractTranscoder/KEY_WIDTH (float width-px))
       (.addTranscodingHint SVGAbstractTranscoder/KEY_HEIGHT (float height-px))
       (.addTranscodingHint SVGAbstractTranscoder/KEY_RESOLUTION_DPI (float 96))
       (.addTranscodingHint SVGAbstractTranscoder/KEY_EXECUTE_ONLOAD Boolean/TRUE)
       (.addTranscodingHint SVGAbstractTranscoder/KEY_ALLOWED_SCRIPT_TYPES "")
       (.addTranscodingHint SVGAbstractTranscoder/KEY_CONSTRAIN_SCRIPT_ORIGIN Boolean/TRUE)
       (.addTranscodingHint SVGAbstractTranscoder/KEY_ALLOW_EXTERNAL_RESOURCES Boolean/TRUE)
       (.transcode (TranscoderInput. document) nil)))))


