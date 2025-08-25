(ns org.mushin.multimedia.img
  (:require [clojure.java.io :as io])
  (:import [io.sf.carte.echosvg.transcoder TranscoderInput TranscoderException TranscoderOutput SVGAbstractTranscoder]
           [io.sf.carte.echosvg.transcoder.image PNGTranscoder]
           [io.sf.carte.echosvg.anim.dom SAXSVGDocumentFactory]
           [org.w3c.dom.svg SVGDocument]
           [javax.imageio ImageIO ImageWriter]
           [java.awt.geom AffineTransform]
           [java.awt.image BufferedImage RenderedImage]
           [java.awt Color]
           [org.svg2gif BufferedImageTranscoder]))

(defn get-img
  [file-path]
  (with-open [file (io/input-stream file-path)]

    (ImageIO/read file)))

(defn write-img-to
  [^BufferedImage img file-path]
  (with-open [file (io/output-stream file-path)
              ios (ImageIO/createImageOutputStream file)]
    (ImageIO/write img "PNG" ios)))

(defn copy-frame
  [{:keys [frame x-offset y-offset] :as full-frame} ^AffineTransform base-transform width height]
  (assoc full-frame :frame (copy-img frame (mul base-transform (AffineTransform/getTranslateInstance x-offset y-offset)) width height)))

(defn copy-frame-map
  [frames ^AffineTransform base-transform width height]
  (mapv #(copy-frame % base-transform width height) frames))

(defn mul
  [^AffineTransform left ^AffineTransform right]
  (doto (.clone left)
    (.concatenate right)))
