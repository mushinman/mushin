(ns org.mushin.multimedia.img
  (:require [clojure.java.io :as io]
            [org.mushin.buffers :as buffers]
            [org.mushin.files :as files]
            [org.mushin.digest :as digest])
  (:import [javax.imageio ImageIO ImageWriter ImageWriteParam IIOImage]
           [javax.imageio.stream ImageInputStream ImageOutputStream]
           [java.security MessageDigest]
           [java.awt.image BufferedImage DataBufferByte WritableRaster]
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

(defn get-img
  [file-path]
  (with-open [file (io/input-stream file-path)]

    (ImageIO/read file)))

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

(defn digest-img!
   "Digests `img`'s RGB data using the default image digest (SHA-256)."
  ^MessageDigest
  ([^BufferedImage img ^MessageDigest md]
   (let [width (.getWidth img)
         height (.getHeight img)
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

(defn checksum-image
  ^bytes
  [^BufferedImage img]
  (digest/digest->bytes (digest-img! img)))
