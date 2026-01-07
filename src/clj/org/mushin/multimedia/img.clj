(ns org.mushin.multimedia.img
  (:require [clojure.java.io :as io]
            [org.mushin.buffers :as buffers]
            [org.mushin.with-disposable :refer [with-disposable]]
            [org.mushin.files :as files]
            [org.mushin.digest :as digest])
  (:import [javax.imageio ImageIO ImageWriter ImageWriteParam IIOImage]
           [javax.imageio.stream ImageInputStream ImageOutputStream]
           [java.io InputStream]
           [java.security MessageDigest]
           [java.util Hashtable]
           [java.awt.image BufferedImage DataBufferByte WritableRaster IndexColorModel DataBuffer ColorModel]
           [java.nio ByteOrder]))

(defn get-raster
  ^WritableRaster
  [^BufferedImage img]
  (.getRaster img))

(defn get-data-byte-buffer
  ^DataBufferByte
  [^WritableRaster raster]
  (.getDataBuffer raster))

(defn get-byte-data
  ^bytes
  [^DataBufferByte buffer]
  (.getData buffer))

(def img-type-argb BufferedImage/TYPE_INT_ARGB)

(def img-type-byte-index BufferedImage/TYPE_BYTE_INDEXED)

(defn img-width
  ^Integer
  [^BufferedImage img]
  (.getWidth img))

(defn img-height
  ^Integer
  [^BufferedImage img]
  (.getHeight img))

(defn create-buffered-img-from-raster
  ^BufferedImage
  [^ColorModel cm ^WritableRaster raster ^Boolean is-raster-premultiplied ^Hashtable properties]
  (BufferedImage. cm raster is-raster-premultiplied properties))

(defn get-width
  ^Integer
  [^BufferedImage img]
  (.getWidth img))

(defn get-height
  ^Integer
  [^BufferedImage img]
  (.getHeight img))

(defn get-color-model
  ^ColorModel
  [^BufferedImage img]
  (.getColorModel img))

(defn copy-buffered-image
  ^BufferedImage
  [^BufferedImage img]
  (let [color-model (get-color-model img)
        raster (.copyData color-model nil)]
    (BufferedImage. color-model raster (.isAlphaPremultiplied color-model) nil)))

(defn coerce-to-buffered-image
  [o]
  (if (instance? BufferedImage o)
    o

    (if-let [f (files/coerce-to-file o)]
      ;; Open from a file.
      (ImageIO/read f)
      (cond
        ;; Already a stream:
        
        (instance? InputStream o)
        (ImageIO/read ^InputStream o)

        (instance? ImageInputStream o)
        (ImageIO/read ^ImageInputStream o)

        :else (throw (ex-info "Unsupported type" {:obj o
                                                  :type (class o)}))))))

(defn get-image-metadata
  [image]
  ;; TODO I'm pretty sure ImageIO supports reading metadata without actually
  ;; opening the entire image.
  (let [img ^BufferedImage (coerce-to-buffered-image image)]
    {:width (get-width img)
     :height (get-height img)
     :color-model (get-color-model img)}))



(defn ->buffered-image
  ^BufferedImage
  ([^Integer width ^Integer height ^Integer image-type ^IndexColorModel cm]
   (BufferedImage. width height image-type cm))
  ([^Integer width ^Integer height ^Integer image-type]
   (BufferedImage. width height image-type))
  ([o]
   ;; Coerce o to a bufferedimage.
   (if (instance? BufferedImage o)
     ;; Copy the image.
     (copy-buffered-image o)

     (coerce-to-buffered-image o))))

(defn digest-img!
   "Digests `img`'s RGB data using the default image digest (SHA-256)."
  ^MessageDigest
  ([^BufferedImage img ^MessageDigest md]
   (let [width (get-width img) 
         height (get-height img) 
         rgb-array (int-array width)
         raw-byte-array (byte-array (* width 4))
         bb (buffers/set-byte-order (buffers/wrap-bytes raw-byte-array) ByteOrder/BIG_ENDIAN)]
     ;; TODO add certain metadata support like EXIF orientation. Would need to rotate the image
     ;; and add that information to the checksum.
     (dotimes [y height]
       (.getRGB img 0 y width 1 rgb-array 0 width)
       (buffers/copy-ints-to-byte-buffer! rgb-array bb)
       (digest/update-digest-buffer md raw-byte-array)
       (buffers/clear-byte-buffer! bb))
     md))
  ([^BufferedImage img]
   (digest-img! img (digest/create-sha256-digest))))


(defn ints-to-byte-icm
  ^IndexColorModel
  [^ints colors transparent-index]
  (IndexColorModel. 8 (alength colors) colors 0 (boolean transparent-index)
                    ^Integer (if transparent-index
                               (int transparent-index)
                               -1) DataBuffer/TYPE_BYTE))

(defn rgb-bytes-to-byte-icm
  ^IndexColorModel
  [^bytes colors transparent-index]
  (IndexColorModel. 8 (/ (alength colors) 3) colors 0 false
                    ^Integer (if transparent-index
                               (int transparent-index)
                               -1)))

(defn copy-img
  ^BufferedImage
  [^BufferedImage src-img ^BufferedImage dest-img]
  (with-disposable [g2d (.createGraphics dest-img)]
    (.drawImage g2d src-img 0 0 nil)
    dest-img))

(defn get-writer-by-mime-type
  ^ImageWriter
  [^String mime-type]
  (-> (ImageIO/getImageWritersByMIMEType mime-type) iterator-seq first))

(defn write-img-from-mime-type
  [^BufferedImage img ^String mime-type ^ImageOutputStream output-stream & {:keys [compression-level compression-algorithm]}]
  (let [writer (get-writer-by-mime-type mime-type)
        params (let [param (.getDefaultWriteParam writer)]
                 (when compression-level
                     (.setCompressionMode param ImageWriteParam/MODE_EXPLICIT)
                     (.setCompressionQuality param (float compression-level)))
                 (when compression-algorithm
                      (.setCompressionMode param ImageWriteParam/MODE_EXPLICIT)
                      (.setCompressionType param compression-algorithm))
                 param)]
    (try
      (.setOutput writer output-stream)
      (.write writer nil (IIOImage. img nil nil) params)
      (catch Exception e
        (throw e))
      (finally
        (.dispose writer)))))


(defn create-image-output-stream
  ^ImageOutputStream
  [output]
  (ImageIO/createImageOutputStream output))

(defn create-image-input-stream
  ^ImageInputStream
  [input]
  (ImageIO/createImageInputStream input))

(defn write-img-to
  [^BufferedImage img file-path]
  (with-open [file (io/output-stream (files/sanitize-file file-path))
              ios (create-image-output-stream file)]
    (ImageIO/write img "PNG" ios)))


(defn checksum-image
  ^bytes
  [^BufferedImage img]
  (digest/digest->bytes (digest-img! img)))
