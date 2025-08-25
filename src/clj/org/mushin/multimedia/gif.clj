(ns org.mushin.multimedia.gif
  (:require [clojure.java.io :as io]
            [java-time.api :as time]
            [org.svg2gif.colorspace :as cs]
            [org.svg2gif.img :as img])
  (:import [javax.imageio ImageIO ImageWriter ImageReader IIOImage ImageTypeSpecifier]
           [javax.imageio.metadata IIOMetadata IIOMetadataNode]
           [org.apache.commons.math3.ml.clustering KMeansPlusPlusClusterer DoublePoint MultiKMeansPlusPlusClusterer CentroidCluster Clusterable]
           [org.apache.commons.math3.ml.distance EuclideanDistance]
           [org.apache.commons.math3.random MersenneTwister]
           [java.util Random]
           [javax.imageio.stream ImageInputStream]
           [java.awt.image BufferedImage RenderedImage IndexColorModel]
           [java.awt.geom AffineTransform]
           [java.io Closeable]))

(defrecord GifSequenceWriter
           [gif-writer]
  Closeable (close [_] (.endWriteSequence gif-writer)))

(defn- as-closeable [obj close-fn]
  (reify java.io.Closeable
    (close [_] (close-fn obj))))

(defn keyify-disposal-method
  [^String method]
  (cond
    (.equalsIgnoreCase method "none") :none
    (.equalsIgnoreCase method "doNotDispose") :do-not-dispose
    (.equalsIgnoreCase method "restoreToBackgroundColor") :restore-to-background-color
    (.equalsIgnoreCase method "restoreToPrevious") :restore-to-previous
    (.equalsIgnoreCase method "undefinedDisposalMethod4") :undefined-disposal-method-4
    (.equalsIgnoreCase method "undefinedDisposalMethod5") :undefined-disposal-method-5
    (.equalsIgnoreCase method "undefinedDisposalMethod6") :undefined-disposal-method-6
    (.equalsIgnoreCase method "undefinedDisposalMethod7") :undefined-disposal-method-7
    :else (throw (ex-info "Invalid input"
                          {:error-type :invalid-argument
                           :invalid-value method
                           :expected-range ["none" "doNotDispose" "restoreToBackgroundColor" "restoreToPrevious"
                                            "undefinedDisposalMethod4" "undefinedDisposalMethod5" "undefinedDisposalMethod6"
                                            "undefinedDisposalMethod7"]}))))
(defn- get-node!
  ^IIOMetadataNode
  [^IIOMetadataNode root ^String node-name]
  (if-let [node (loop [num-nodes (.getLength root)
                       i 0]
                  (cond
                    (= i num-nodes) false

                    (.equalsIgnoreCase (-> (.item root i)
                                           (.getNodeName))
                                       node-name) (.item root i)

                    :else (recur num-nodes (inc i))))]
    node
    (let [node (IIOMetadataNode. node-name)]
      (.appendChild root node)
      node)))

(defn- get-reader
  ^ImageReader
  []
  (-> (ImageIO/getImageReadersBySuffix "gif") iterator-seq first))

(defn- get-writer
  ^ImageWriter
  []
  (-> (ImageIO/getImageWritersBySuffix "gif") iterator-seq first))

(defn get-gif-timeframes
  "Create a sequence of frame times for each frame in `gif`"
  [gif]
  (let [reader (get-reader)]
    (with-open [_ (as-closeable reader #(.dispose %))
                iis (ImageIO/createImageInputStream gif)]
      (.setInput reader iis)
      (mapv (fn [^long i]
              (let [frame-metadata (.getImageMetadata reader i)
                    root (.getAsTree frame-metadata (.getNativeMetadataFormatName frame-metadata))
                    gce (.item (.getElementsByTagName root "GraphicControlExtension") 0)]
                ;; Here we clamp to 1 ms because it's technically possible for a gif to have a frame time of 0ms.
                (time/duration (max (Integer/parseInt (.getAttribute gce "delayTime")) 1) :millis)))
            (range (.getNumImages reader true))))))

(defn get-gif-frames
  [^ImageInputStream gif-stream]
  (let [reader (doto (get-reader)
                 (.setInput gif-stream))]
    (mapv (fn [^long i]
            (let [frame-metadata (.getImageMetadata reader i)
                  root (.getAsTree frame-metadata (.getNativeMetadataFormatName frame-metadata))
                  gce (.item (.getElementsByTagName root "GraphicControlExtension") 0)]
                ;; Here we clamp to 1 ms because it's technically possible for a gif to have a frame time of 0ms.
              {:x-offset (Integer/parseInt (-> (get-node! root "ImageDescriptor")
                                               (.getAttributes)
                                               (.getNamedItem "imageLeftPosition")
                                               (.getNodeValue)))
               :y-offset (Integer/parseInt (-> (get-node! root "ImageDescriptor")
                                               (.getAttributes)
                                               (.getNamedItem "imageTopPosition")
                                               (.getNodeValue)))

               :duration (time/duration (max (Integer/parseInt (.getAttribute gce "delayTime")) 1) :millis)
               :frame    (.read reader i)
               :disposal (or (.getAttribute gce "disposalMethod") "none")}))
          (range (.getNumImages reader true)))))

(defn write-to-sequence!
  [^GifSequenceWriter writer {:keys [duration frame disposal]}]
  (let [writer (:gif-writer writer)
        write-param (.getDefaultWriteParam writer)]
    (.writeToSequence writer (IIOImage. frame nil
                                        (let [metadata (.getDefaultImageMetadata writer
                                                                                 (ImageTypeSpecifier/createFromRenderedImage frame)
                                                                                 write-param)
                                              fmt (.getNativeMetadataFormatName metadata)
                                              root (.getAsTree metadata fmt)]
                                          (doto (.item (.getElementsByTagName root "GraphicControlExtension") 0)
                                            (.setAttribute "disposalMethod" disposal)
                                            (.setAttribute "userInputFlag" "FALSE")
                                            (.setAttribute "delayTime" (str (time/as duration :millis))))
                                          ;; Apply local indexed color table.
                                          (when (= BufferedImage/TYPE_BYTE_INDEXED (.getType frame))
                                            (let [^IndexColorModel color-model (.getColorModel frame)
                                                  map-size (.getMapSize color-model)
                                                  lct (doto (IIOMetadataNode. "LocalColorTable")
                                                        (.setAttribute "sortFlag" "FALSE")
                                                        (.setAttribute "sizeOfLocalColorTable" (str map-size))
                                                        ;(.setAttribute "size" (.getMapSize color-model))
                                                       ; (.setAttribute "bitsPerPixel" "8")
                                                        )] ; TODO support other sizes
                                              (dotimes [n map-size]
                                                (.appendChild lct
                                                              (doto (IIOMetadataNode. "ColorTableEntry")
                                                                (.setAttribute "index" (str n))
                                                                (.setAttribute "red" (str (.getRed color-model (int n))))
                                                                (.setAttribute "green" (str (.getGreen color-model (int n))))
                                                                (.setAttribute "blue" (str (.getBlue color-model (int n)))))))
                                              (.appendChild root lct)))
                                          ;; Processed by mushin.
                                          (doto (get-node! root "CommentExtensions")
                                            (.setAttribute "CommentExtension" "Created by mushin"))
                                          (doto (get-node! root "ApplicationExtensions")
                                            (.appendChild (doto (IIOMetadataNode. "ApplicationExtension")
                                                            (.setAttribute "applicationID" "NETSCAPE")
                                                            (.setAttribute "authenticationCode" "2.0")
                                                            (.setUserObject (byte-array [0x01 0x00 0x00]))))) ; Infinite loop.
                                          (.setFromTree metadata fmt root)
                                          metadata))
                      write-param)))

(defn write-frames-to-sequence!
  [^GifSequenceWriter writer frames]
  (when-not (empty? frames)
    (write-to-sequence! writer (first frames))
    (recur writer (rest frames))))

(defn create-gif-sequence
  [output-stream]
  (GifSequenceWriter. (doto (get-writer)
                        (.setOutput output-stream)
                        (.prepareWriteSequence nil))))

(defn get-gif-from-path
  [src-path]
  (with-open [source-file (io/input-stream src-path)
              gif-iis     (ImageIO/createImageInputStream source-file)]
    (get-gif-frames gif-iis)))

(defn gif-deep-copy
  [src-path dest-path]
  (with-open [dest-file   (io/output-stream dest-path)
              gif-ios     (ImageIO/createImageOutputStream dest-file)
              writer      (create-gif-sequence gif-ios)]
    (let [frames (get-gif-from-path src-path)
          width (.getWidth (:frame (first frames)))
          height (.getHeight (:frame (first frames)))]
      (write-frames-to-sequence! writer (img/copy-frame-map frames (AffineTransform.) width height)))))

(defn apply-caption-to-frame
  [caption-img caption-stop-y frame]
  (let [combined-img (BufferedImage. (.getWidth caption-img) (.getHeight caption-img) BufferedImage/TYPE_INT_ARGB)]
    (doto (.getGraphics combined-img)
      (.drawRenderedImage caption-img (AffineTransform.))
      (.drawRenderedImage frame (AffineTransform/getTranslateInstance 0 caption-stop-y))
      (.dispose))
    combined-img))

(defn create-frame
  [content duration disposal]
  {:frame content
   :duration duration
   :disposal disposal})

(defn gif-caption
  [caption-img caption-stop-y src-path dest-path]
  (with-open [dest-file   (io/output-stream dest-path)
              gif-ios     (ImageIO/createImageOutputStream dest-file)
              writer      (create-gif-sequence gif-ios)]
    (let [frames (get-gif-from-path src-path)
          width (.getWidth caption-img)
          height (.getHeight caption-img)
          first-frame (first frames)]
      (write-frames-to-sequence! writer (concat
                                         [(create-frame (apply-caption-to-frame caption-img caption-stop-y (:frame first-frame)) (time/duration 1 :millis) "none")]
                                         (img/copy-frame-map
                                          frames
                                          (AffineTransform/getTranslateInstance 0 caption-stop-y) width height))))))

(defn- find-closest-palette-match-index
  ^Integer
  [^EuclideanDistance measurer pixel palette]
  ;; Find the nearest color in the palette with brute force.
  (loop [;; Index/iterator.
         i 0
         ;; Index to the current nearest match.
         match-index 0
         ;; Distance of the hue referenced by match-index to pixel.
         closest-distance Double/POSITIVE_INFINITY]
    (if (= i (count palette))
      match-index
      (let [next-hue (nth palette i)
            next-hue-distance (.compute measurer next-hue pixel)
            closer (< next-hue-distance closest-distance)]
        (recur (inc i)
               (if closer
                 i
                 match-index)
               (if closer
                 next-hue-distance
                 closest-distance))))))

(defn quantize-image
  ^BufferedImage
  [^BufferedImage img]
  (let [width (.getWidth img)
        height (.getHeight img)
        ;; Raw ARGB byte array from the image.
        rgb-stream (.getRGB img 0 0 width height nil 0 width)
        ;; Vector of double-arrays containing the CIELAB values for each pixel.
        ;; Or nil, if the pixel is transparent.
        lab-stream (mapv (fn [^Integer i]
                           ;; If the pixel is opaque then convert to lab.
                           ;; Otherwise, nil.
                           (if (= (bit-and (bit-shift-right i 24) 0xFF) 0xFF)
                             (double-array (cs/srgb->cielab (/ (bit-and (bit-shift-right i 16) 0xFF) 255.0)
                                                            (/ (bit-and (bit-shift-right i 8) 0xFF) 255.0)
                                                            (/ (bit-and i 0xFF) 255.0)))
                             nil))
                         rgb-stream)
        opaque-pixel-count (count (filter some? lab-stream))
        has-transparent-pixels (< opaque-pixel-count (count lab-stream))
        max-unique-colors (if has-transparent-pixels
                            255
                            256)
        ;; A sample of lab-stream excluding transparent pixels.
        image-sample (into [] (comp
                               (filter some?)
                               (random-sample (min 1.0 (/  200000.0 opaque-pixel-count))) ; TODO if the image is entirely tansparent or empty that will result in a div by 0.
                               (map #(DoublePoint. %)))
                           lab-stream)

        distance-measurer (EuclideanDistance.)
        cluster (.cluster (MultiKMeansPlusPlusClusterer.
                           (KMeansPlusPlusClusterer.
                            max-unique-colors
                            20
                            distance-measurer
                            (MersenneTwister.))
                           3)
                          image-sample)
        ;; Vector of double-arrays in CIELAB.  A quantized version image-sample.
        palette-lab (mapv (fn [^CentroidCluster centroid]
                            (.getPoint (.getCenter centroid)))
                          cluster)
        ;; A byte-array, with each byte being a red, green, or red component interleaved.  An sRGB of palette-lab.
        palette-rgb (byte-array (concat
                                 (flatten (mapv (fn [[l a b]]
                                                  (let [[r g b] (cs/cielab->srgb l a b)]
                                                    [(cs/normalized->byte r) (cs/normalized->byte g) (cs/normalized->byte b)]))
                                                palette-lab))
                                 ;; Add an extra transparency pixel on.
                                 (when has-transparent-pixels
                                   [(byte 0) (byte 0) (byte 0)])))
        palettized-image (BufferedImage. width height BufferedImage/TYPE_BYTE_INDEXED
                                         (IndexColorModel. 8 256 palette-rgb 0 false (if has-transparent-pixels
                                                                                       255
                                                                                       -1)))
        ^bytes raster-buffer (.getData (.getDataBuffer (.getRaster palettized-image)))]
    (dotimes [i (alength raster-buffer)]
      (aset-byte raster-buffer i (unchecked-byte
                                  (if-let [pixel (nth lab-stream i)]
                                    (find-closest-palette-match-index distance-measurer pixel palette-lab)
                                    255))))
    palettized-image))
