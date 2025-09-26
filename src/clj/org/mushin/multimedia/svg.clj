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
           [io.sf.carte.echosvg.dom.util DOMUtilities]
           [org.w3c.dom.svg SVGDocument SVGFitToViewBox]
           [javax.imageio ImageIO ImageWriter]
           [java.io ByteArrayInputStream OutputStream]
           [javax.xml.transform.stream StreamResult]
           [javax.xml.transform.dom DOMSource]
           [java.awt.geom AffineTransform]
           [java.awt.image BufferedImage RenderedImage]
           [java.io StringWriter]
           [org.mushin.multimedia BufferedImageTranscoder]))


(def mushin-ns "urn:mushin")

(def ^String svg-ns SVGDOMImplementation/SVG_NAMESPACE_URI)


(defn get-caption-pixel-height
  [virtual-height content-virtual-height content-pixel-height]
  (* (/ (- virtual-height content-virtual-height) content-virtual-height) content-pixel-height))

(defn get-content-virtual-height
  [content-virtual-width content-pixel-width content-pixel-height]
  (* content-virtual-width (/ content-pixel-height content-pixel-width)))

(def ^String meme-css
  "#caption {
  font: 80px/1 \"Montserrat\", Montserrat, sans-serif;
  fill: #fff;
  stroke: #fff;
  stroke-width: 0;
  paint-order: stroke fill;
  text-anchor: middle;
  dominant-baseline: middle;
}
#frame {
  vector-effect: non-scaling-stroke;
  fill: #000;
}
#testarea {
  fill: #F00;
}
image { image-rendering: auto; }")



(defn open-svg-doc
  ^SVGDocument
  [^String svg-path]
  (let [factory (SAXSVGDocumentFactory.)]
    (.createDocument factory svg-path)))

(defn ^SVGDocument make-meme-svg
  "Builds an SVG Document."
  [image-pixel-width image-pixel-height content-href caption caption-pixel-height]
  (when (or (nil? image-pixel-width) (nil? image-pixel-height) (<= (double image-pixel-width) 0.0))
    (throw (ex-info "image width/height must be positive" {:pxw image-pixel-width :pxh image-pixel-height})))

  (let [impl (SVGDOMImplementation/getDOMImplementation)
        doc  (.createDocument impl svg-ns "svg" nil)
        root (.getDocumentElement doc)
        virtual-width 1000.0
        img-height (* virtual-width (/ (double image-pixel-height) (double image-pixel-width)))
        caption-virtual-height (* virtual-width (/ (double caption-pixel-height) (double image-pixel-height)))
        total-height (+ caption-virtual-height img-height)]

    (doto root
      (.setAttribute "viewBox" (format "0 0 1000 %.0f" total-height))
      (.setAttribute "width" "100%")
      (.setAttribute "preserveAspectRatio" "xMidYMid meet"))

    (let [style (.createElementNS doc svg-ns "style")]
      (.appendChild style (.createTextNode doc meme-css))
      (.appendChild root style))

    ;; Custom mushin metadata
    (let [md        (.createElementNS doc svg-ns "metadata")
          mushin-el (.createElementNS doc "urn:mushin" "mushin:content-image")]
      (.setAttributeNS mushin-el "http://www.w3.org/2000/xmlns/" "xmlns:mushin" "urn:mushin")
      (.setAttribute mushin-el "id" "mushin-metadata")
      (.setAttributeNS mushin-el "urn:mushin" "mushin:pxw" (str (long image-pixel-width)))
      (.setAttributeNS mushin-el "urn:mushin" "mushin:pxh" (str (long image-pixel-height)))
      (.appendChild md mushin-el)
      (.appendChild root md))


    (let [frame (.createElementNS doc svg-ns "rect")]
      (doto frame
        (.setAttribute "id" "frame")
        (.setAttribute "x" "0")
        (.setAttribute "y" "0")
        (.setAttribute "width" "1000")
        (.setAttribute "height" (str caption-virtual-height)))
      (.appendChild root frame))

    (let [img (.createElementNS doc svg-ns "image")]
      (doto img
        (.setAttribute "x" "0")
        (.setAttribute "y" (str caption-virtual-height))
        (.setAttribute "width" "100%")
        (.setAttribute "height" (format "%.0f" img-height))
        (.setAttribute "href" (str content-href)))
      (.appendChild root img))

    (let [txt (.createElementNS doc svg-ns "text")]
      (doto txt
        (.setAttribute "id" "caption")
        (.setAttribute "x" "500")
        (.setAttribute "y" "90"))
      (.appendChild txt (.createTextNode doc (str caption)))
      (.appendChild root txt))
    doc))

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
