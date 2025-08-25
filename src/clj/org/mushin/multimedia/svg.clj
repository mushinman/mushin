(ns org.mushin.multimedia.svg
  (:require [clojure.walk :refer [keywordize-keys]]
            [clojure.java.io :as io]
            [org.svg2gif.gif :as gif])
  (:import [io.sf.carte.echosvg.transcoder TranscoderInput TranscoderException TranscoderOutput SVGAbstractTranscoder]
           [io.sf.carte.echosvg.transcoder.image PNGTranscoder]
           [io.sf.carte.echosvg.anim.dom SAXSVGDocumentFactory]
           [org.w3c.dom.svg SVGDocument]
           [javax.imageio ImageIO ImageWriter]
           [java.awt.geom AffineTransform]
           [java.awt.image BufferedImage RenderedImage]
           [org.svg2gif BufferedImageTranscoder]))


(defn render
  ^BufferedImage
  [^SVGDocument svg-doc width-px height-px]
  (let [tx (doto (BufferedImageTranscoder.)
             (.addTranscodingHint SVGAbstractTranscoder/KEY_WIDTH (float width-px))
             (.addTranscodingHint SVGAbstractTranscoder/KEY_HEIGHT (float height-px))
             (.addTranscodingHint SVGAbstractTranscoder/KEY_RESOLUTION_DPI (float 96))

             (.addTranscodingHint SVGAbstractTranscoder/KEY_EXECUTE_ONLOAD Boolean/TRUE)

             (.addTranscodingHint SVGAbstractTranscoder/KEY_ALLOWED_SCRIPT_TYPES "")
             (.addTranscodingHint SVGAbstractTranscoder/KEY_CONSTRAIN_SCRIPT_ORIGIN Boolean/TRUE)
             (.addTranscodingHint SVGAbstractTranscoder/KEY_ALLOW_EXTERNAL_RESOURCES Boolean/FALSE)
             (.transcode (TranscoderInput. svg-doc) nil))]
    (.getImage tx)))
