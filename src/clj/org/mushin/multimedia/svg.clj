(ns org.mushin.multimedia.svg
  (:require [clojure.java.io :as io]
            [clojure.math :as math]
            [clojure.string :as str]
            [org.mushin.codecs :as b64]
            [org.mushin.multimedia.gif :as gif]
            [java-time.api :as time])
  (:import [io.sf.carte.echosvg.transcoder TranscoderInput TranscoderException TranscoderOutput SVGAbstractTranscoder]
           [io.sf.carte.echosvg.transcoder.image PNGTranscoder]
           [io.sf.carte.echosvg.anim.dom SVGDOMImplementation]
           [javax.xml.transform TransformerFactory OutputKeys]
           [javax.imageio ImageIO ImageWriter ImageReader IIOImage ImageTypeSpecifier]
           [io.sf.carte.echosvg.anim.dom SAXSVGDocumentFactory]
           [io.sf.carte.echosvg.bridge UserAgentAdapter DocumentLoader BridgeContext GVTBuilder]
           [io.sf.carte.echosvg.gvt GraphicsNode]
           [io.sf.carte.echosvg.dom.util DOMUtilities]
           [org.w3c.dom.svg SVGDocument SVGFitToViewBox SVGElement]
           [javax.imageio ImageIO ImageWriter]
           [java.io ByteArrayInputStream OutputStream]
           [javax.xml.transform.stream StreamResult]
           [javax.xml.transform.dom DOMSource]
           [java.awt.geom AffineTransform Rectangle2D]
           [java.awt.image BufferedImage RenderedImage]
           [java.io StringWriter]
           [org.mushin.multimedia BufferedImageTranscoder]))


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

(defn doc->stream
  [^SVGDocument doc ^OutputStream stream]
  (let [tf (TransformerFactory/newInstance)
        tr (.newTransformer tf)]
    (.setOutputProperty tr OutputKeys/OMIT_XML_DECLARATION "no")
    (.setOutputProperty tr OutputKeys/METHOD "xml")
    (.setOutputProperty tr OutputKeys/ENCODING "UTF-8")
    (.setOutputProperty tr OutputKeys/INDENT "yes")
    (.transform tr (DOMSource. doc) (StreamResult. stream))
    stream))

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
     (doto (BufferedImageTranscoder.)
       (.addTranscodingHint SVGAbstractTranscoder/KEY_WIDTH (float width-px))
       (.addTranscodingHint SVGAbstractTranscoder/KEY_HEIGHT (float height-px))
       (.addTranscodingHint SVGAbstractTranscoder/KEY_RESOLUTION_DPI (float 96))
       (.addTranscodingHint SVGAbstractTranscoder/KEY_EXECUTE_ONLOAD Boolean/TRUE)
       (.addTranscodingHint SVGAbstractTranscoder/KEY_ALLOWED_SCRIPT_TYPES "")
       (.addTranscodingHint SVGAbstractTranscoder/KEY_CONSTRAIN_SCRIPT_ORIGIN Boolean/TRUE)
       (.addTranscodingHint SVGAbstractTranscoder/KEY_ALLOW_EXTERNAL_RESOURCES Boolean/FALSE)
       (.transcode (TranscoderInput. document) nil)))))

(def href-base64 "base64,")

(defn clone-document
  [svg-doc]
  (let [clone (DOMUtilities/deepCloneDocument svg-doc (.getImplementation svg-doc))]
    (.setDocumentURI clone (.getDocumentURI svg-doc))
    clone))


(defn render-gif
  [svg-file gif-file]
  (let [svg-doc (open-svg-doc svg-file)
        virtual-height (let [{:keys [height]} (get-viewbox-of (.getDocumentElement svg-doc))]
                         height)
        md (.getElementById svg-doc "mushin-metadata")

        content-pixel-width (double (Integer/parseInt (.getAttributeNS md mushin-ns "pxw")))
        content-pixel-height (double  (Integer/parseInt (.getAttributeNS md mushin-ns "pxh")))

        image-node (.getElementById svg-doc "media")
        content-virtual-height (double (Integer/parseInt (.getAttribute image-node "height")))
        ;; TODO optimization turn that into a memorystream.
        gif-stream  (let [href (.getAttribute image-node "href")
                          base64-pos (str/index-of href href-base64)]
                      (b64/b64->bytes (when base64-pos
                                        (subs href (+ base64-pos (count href-base64))))))

        caption-pixel-height (math/ceil (get-caption-pixel-height virtual-height content-virtual-height content-pixel-height))

        gif-frames (let [gif (with-open [memstream (ByteArrayInputStream. gif-stream)]
                               (gif/get-gif-from-stream memstream))]
                     (assoc gif :scenes
                            (mapv (fn [{:keys [x-offset y-offset] :as frame}]
                                    (let [offset (AffineTransform/getTranslateInstance 0 caption-pixel-height)
                                          in-image (AffineTransform/getTranslateInstance x-offset y-offset)]
                                      (.concatenate offset in-image)
                                      (assoc frame :x-offset (int (.getTranslateX offset)) :y-offset (int (.getTranslateY offset)))))
                                  (:scenes gif))))
        ;; Create a cloned document from the original and remove the image content because
        ;; echosvg cannot render GIFs.
        cloned-document (let [new-doc (clone-document svg-doc)
                              img-node (.getElementById new-doc "media")]
                          (.removeAttribute img-node "href")
                          new-doc)
        caption-img (render-document cloned-document content-pixel-width (+ content-pixel-height caption-pixel-height))
        initial-frame (gif/apply-caption-to-frame caption-img caption-pixel-height (gif/get-first-frame gif-frames))
        quanted-image (gif/quantize-image initial-frame)
        epic (gif/add-scene gif-frames {:x-offset 0 :y-offset 0 :colortable-is-local true :disposal "doNotDispose" :frame quanted-image :duration (time/duration 10 :millis)})]
    (with-open [os (io/output-stream gif-file)
                gif-ios (ImageIO/createImageOutputStream os)]
      (gif/write-gif-to gif-ios epic))))
