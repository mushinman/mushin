(ns org.mushin.multimedia.img
  (:require [clojure.java.io :as io]
            [org.mushin.buffers :as buffers]
            [org.mushin.digest :as digest])
  (:import [io.sf.carte.echosvg.transcoder TranscoderInput TranscoderException TranscoderOutput SVGAbstractTranscoder]
           [io.sf.carte.echosvg.transcoder.image PNGTranscoder]
           [io.sf.carte.echosvg.anim.dom SAXSVGDocumentFactory]
           [org.w3c.dom.svg SVGDocument]
           [javax.imageio ImageIO ImageWriter]
           [java.awt.geom AffineTransform]
           [java.awt.image BufferedImage RenderedImage]
           [java.awt Color]
           [org.mushin.multimedia BufferedImageTranscoder]
           [java.nio ByteOrder]))

(defn get-img
  [file-path]
  (with-open [file (io/input-stream file-path)]

    (ImageIO/read file)))

(defn write-img-to
  [^BufferedImage img file-path]
  (with-open [file (io/output-stream file-path)
              ios (ImageIO/createImageOutputStream file)]
    (ImageIO/write img "PNG" ios)))

(defn checksum-image
  [^BufferedImage img]
  (let [width (.getWidth img)
        height (.getHeight img)
        rgb-array (int-array width)
        raw-byte-array (byte-array (* width 4))
        md (digest/create-sha256-digest)
        bb (buffers/set-byte-order (buffers/wrap-bytes raw-byte-array) ByteOrder/BIG_ENDIAN)]
    ;; TODO add certain metadata support like EXIF orientation. Would need to rotate the image
    ;; and add that information to the checksum.
    (dotimes [y height]
      (.getRGB img 0 y width 1 rgb-array 0 width)
      (buffers/copy-ints-to-byte-buffer! rgb-array bb)
      (digest/update-digest-buffer md raw-byte-array)
      (buffers/clear-byte-buffer bb))
    (digest/digest->b64 md)))

;(defn copy-frame
;  [{:keys [frame x-offset y-offset] :as full-frame} ^AffineTransform base-transform width height]
;  (assoc full-frame :frame (copy-img frame (mul base-transform (AffineTransform/getTranslateInstance x-offset y-offset)) width height)))

;(defn copy-frame-map
;  [frames ^AffineTransform base-transform width height]
;  (mapv #(copy-frame % base-transform width height) frames))

(defn mul
  [^AffineTransform left ^AffineTransform right]
  (doto (.clone left)
    (.concatenate right)))
