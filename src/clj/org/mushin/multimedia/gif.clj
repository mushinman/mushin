(ns org.mushin.multimedia.gif
  (:require [clojure.java.io :as io]
            [java-time.api :as time]
            [org.mushin.utils :refer [condj]]
            [org.mushin.geom :as geom]
            [org.mushin.with-disposable :refer [with-disposable]]
            [org.mushin.files :as files]
            [org.mushin.multimedia.svg :as svg]
            [org.mushin.multimedia.captions :as caption]
            [clojure.math :as math]
            [org.mushin.multimedia.colorspace :as cs]
            [org.mushin.multimedia.img :as img])
  (:import [javax.imageio ImageIO ImageWriter ImageReader IIOImage ImageTypeSpecifier]
           [javax.imageio.metadata IIOMetadataNode]
           [org.apache.commons.math3.ml.clustering KMeansPlusPlusClusterer DoublePoint MultiKMeansPlusPlusClusterer CentroidCluster Clusterable]
           [org.apache.commons.math3.ml.distance EuclideanDistance]
           [org.apache.commons.math3.random MersenneTwister]
           [org.w3c.dom Element]
           [java.time Duration]
           [javax.imageio.stream ImageInputStream ImageOutputStream]
           [java.awt Transparency]
           [java.awt.image BufferedImage IndexColorModel]))

(defonce logical-screen-descriptor "LogicalScreenDescriptor")

(def graphic-control-extension "GraphicControlExtension")

;; (defrecord GifSequenceWriter
;;     [^ImageWriter gif-writer]
;;   Closeable (close [_] (.endWriteSequence gif-writer)))

(defn- ceilint
  [^double d]
  (int (math/ceil d)))

(defn- roundint
  [^double d]
  (int (math/round d)))

(defn centiseconds
  [centis]
  (time/duration (* centis 10) :millis))

(defn duration-to-centiseconds
  [^Duration duration]
  (/ (time/as duration :millis) 10))

(defn- is-indexed
  ^Boolean
  [^BufferedImage image]
  (or (= (.getType image) BufferedImage/TYPE_BYTE_INDEXED)
      (= (.getType image) BufferedImage/TYPE_BYTE_BINARY)))

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

(defn- get-node
  ^IIOMetadataNode
  [^IIOMetadataNode root ^String node-name]
  (let [nl (.getElementsByTagName root node-name)]
    (when (pos? (.getLength nl))
      (.item nl 0))))

(defn- get-reader
  ^ImageReader
  []
  (-> (ImageIO/getImageReadersBySuffix "gif") iterator-seq first))

(defn- get-writer
  ^ImageWriter
  []
  (-> (ImageIO/getImageWritersBySuffix "gif") iterator-seq first))

(defn- get-gif-colortable-size
  ^Integer
  [^Integer n]
  (cond
    (= n 1) 1
    (= n 2) 2
    (<= n 4) 4
    (<= n 8) 8
    (<= n 16) 16
    (<= n 32) 32
    (<= n 64) 64
    (<= n 128) 128
    (<= n 256) 256
    :else (throw (ex-info "Invalid value for a gif color table size (must be a power of 2 and not exceeding 256)" {:n n}))))

(defn- write-color-model!
  ^IIOMetadataNode
  [^IndexColorModel icm ^IIOMetadataNode node]
  (let [map-size (.getMapSize icm)]
    (dotimes [n map-size]
      (.appendChild node
                    (doto (IIOMetadataNode. "ColorTableEntry")
                      (.setAttribute "index" (str n))
                      (.setAttribute "red" (str (.getRed icm (int n))))
                      (.setAttribute "green" (str (.getGreen icm (int n))))
                      (.setAttribute "blue" (str (.getBlue icm (int n)))))))
    (dotimes [n (- (get-gif-colortable-size map-size) map-size)]
      (.appendChild node
                    (doto (IIOMetadataNode. "ColorTableEntry")
                      (.setAttribute "index" (str (+ map-size n)))
                      (.setAttribute "red" "0")
                      (.setAttribute "green" "0")
                      (.setAttribute "blue" "0"))))
    node))

(defn get-gif-timeframes
  "Create a sequence of frame times for each frame in `gif`"
  [gif]
  (with-disposable [reader (get-reader)]
    (with-open [iis (ImageIO/createImageInputStream gif)]
      (.setInput reader iis)
      (mapv (fn [^long i]
              (let [frame-metadata (.getImageMetadata reader i)
                    root (.getAsTree frame-metadata (.getNativeMetadataFormatName frame-metadata))
                    gce (get-node root "GraphicControlExtension")]
                ;; Here we clamp to 1 ms because it's technically possible for a gif to have a frame time of 0ms.
                (centiseconds (max (Integer/parseInt (.getAttribute gce "delayTime")) 1))))
            (range (.getNumImages reader true))))))

(defn get-gif-frames-from-reader
  [^ImageReader reader]
  (mapv (fn [^long i]
          (let [frame-metadata (.getImageMetadata reader i)
                ^Element root (.getAsTree frame-metadata (.getNativeMetadataFormatName frame-metadata))
                gce (get-node root "GraphicControlExtension")]
            ;; Here we clamp to 1 ms because it's technically possible for a gif to have a frame time of 0ms.
            (cond-> {:x-offset (Integer/parseInt (-> (get-node! root "ImageDescriptor")
                                                     (.getAttributes)
                                                     (.getNamedItem "imageLeftPosition")
                                                     (.getNodeValue)))
                     :y-offset (Integer/parseInt (-> (get-node! root "ImageDescriptor")
                                                     (.getAttributes)
                                                     (.getNamedItem "imageTopPosition")
                                                     (.getNodeValue)))
                     :colortable-is-local (pos? (.getLength (.getElementsByTagName root "LocalColorTable")))
                     :duration (centiseconds (max (Integer/parseInt (.getAttribute gce "delayTime")) 1))
                     :frame    (.read reader i)
                     :disposal (or (.getAttribute gce "disposalMethod") "none")}
              (and (some? gce) (= (.getAttribute gce "transparentColorFlag") "TRUE")) (assoc :transparent-color-index (Integer/parseInt (.getAttribute gce "transparentColorIndex"))))))
        (range (.getNumImages reader true))))

(defn get-gif-frames
  [^ImageInputStream gif-stream]
  (let [reader (doto (get-reader)
                 (.setInput gif-stream))]
    (get-gif-frames-from-reader reader)))


(defn write-to-sequence!
  [^ImageWriter writer {:keys [^Duration duration ^BufferedImage frame disposal x-offset y-offset colortable-is-local ^IndexColorModel transparent-color-index]}]
  (let [write-param (.getDefaultWriteParam writer)]
    (.writeToSequence writer (IIOImage. frame nil
                                        (let [metadata (.getDefaultImageMetadata writer
                                                                                 (ImageTypeSpecifier/createFromRenderedImage frame)
                                                                                 write-param)
                                              fmt (.getNativeMetadataFormatName metadata)
                                              ^Element root (.getAsTree metadata fmt)
                                              gce (get-node root graphic-control-extension)]
                                          (doto gce
                                            (.setAttribute "disposalMethod" disposal)
                                            (.setAttribute "userInputFlag" "FALSE")
                                            (.setAttribute "delayTime" (str (duration-to-centiseconds duration))))
                                          (let [id (doto (get-node! root "ImageDescriptor")
                                                     (.setAttribute "imageLeftPosition" (str x-offset))
                                                     (.setAttribute "imageTopPosition" (str y-offset)))]
                                            (when-not colortable-is-local
                                              (.setAttribute id "hasLocalColorTable" "FALSE")))
                                          (doto (get-node! root "ApplicationExtensions")
                                            (.appendChild (doto (IIOMetadataNode. "ApplicationExtension")
                                                            (.setAttribute "applicationID" "NETSCAPE")
                                                            (.setAttribute "authenticationCode" "2.0")
                                                            (.setUserObject (byte-array [0x01 0x00 0x00]))))) ; Infinite loop.

                                          (when-not colortable-is-local
                                            ;; Handle any global color table specific logic.
                                            (when-let [lct (get-node root "LocalColorTable")]
                                              ;; ImageIO usually creates a LCT by default so we have to destroy it.
                                              (.removeChild root lct))
                                            (if transparent-color-index
                                              ;; Handle transparent color index if using the global color table.
                                              (doto gce
                                                (.setAttribute "transparentColorFlag" "TRUE")
                                                (.setAttribute "transparentColorIndex" "1"));(str transparent-color-index)))
                                              (doto gce
                                                (.setAttribute "transparentColorFlag" "FALSE"))))

                                          (when (and (or (= BufferedImage/TYPE_BYTE_INDEXED (.getType frame))
                                                         (= BufferedImage/TYPE_BYTE_BINARY (.getType frame)))
                                                     colortable-is-local)
                                            ;; Copy the index color model from the source frames to the destination frames
                                            ;; (imageio doesn't do this automatically for some reason).
                                            (let [^IndexColorModel color-model (.getColorModel frame)
                                                  map-size (get-gif-colortable-size (.getMapSize color-model))
                                                  lct (doto (IIOMetadataNode. "LocalColorTable")
                                                        (.setAttribute "sortFlag" "FALSE")
                                                        (.setAttribute "sizeOfLocalColorTable" (str map-size)))]
                                              (if (= Transparency/BITMASK (.getTransparency color-model))
                                                ;; Handle transparent color index if using a local color table.
                                                (doto gce
                                                  (.setAttribute "transparentColorFlag" "TRUE")
                                                  (.setAttribute "transparentColorIndex" (str (.getTransparentPixel color-model))))
                                                (doto gce
                                                  (.setAttribute "transparentColorFlag" "FALSE")))

                                              (write-color-model! color-model lct)
                                              (.appendChild root lct)))
                                          (.setFromTree metadata fmt root)
                                          metadata))
                      write-param)))

(defn write-frames-to-sequence!
  [^ImageWriter writer frames]
  (doseq [frame frames]
    (write-to-sequence! writer frame)))

(defn create-gif-reader
  ^ImageReader
  [^ImageInputStream iis]
  (doto (get-reader)
    (.setInput iis)))

(defn create-gif-sequence
  ^ImageWriter
  [^ImageOutputStream output-stream width height background-index ^IndexColorModel global-color-table]
  (when (and background-index (not global-color-table))
    (throw (ex-info "Invalid GIF: Gif defined a background-index but does not have a global-color-table!"
                    {:background-index background-index})))
  (let [writer (get-writer)]
    (doto writer
      (.setOutput output-stream)
      (.prepareWriteSequence
       (let [param (.getDefaultWriteParam writer)
             sm (.getDefaultStreamMetadata writer param)
             fmt (.getNativeMetadataFormatName sm)
             root (IIOMetadataNode. fmt)]
         (.appendChild root (doto (IIOMetadataNode. "Version")
                              (.setAttribute "value" "89a")))
         (.appendChild root
                       (doto (IIOMetadataNode. "LogicalScreenDescriptor")
                         (.setAttribute "logicalScreenWidth" (str width))
                         (.setAttribute "logicalScreenHeight" (str height))
                         (.setAttribute "colorResolution" "8")
                         (.setAttribute "pixelAspectRatio" "0")))
         (when global-color-table
           (let [gct-node (doto (IIOMetadataNode. "GlobalColorTable")
                            (.setAttribute "sizeOfGlobalColorTable" (str (.getMapSize global-color-table)))
                            (.setAttribute "sortFlag" "FALSE")
                            (.setAttribute "backgroundColorIndex" (str (or background-index 0))))]
             (write-color-model! global-color-table gct-node)
             (.appendChild root gct-node)))

         (.setFromTree sm fmt root)
         sm))
      ;; TODO
      )))


(defn write-gif-to-stream
  [^ImageOutputStream output-stream {:keys [background-index global-color-table width height scenes]}]
  (with-disposable [writer (create-gif-sequence output-stream width height background-index global-color-table)]
    (write-frames-to-sequence! writer scenes)
    (.endWriteSequence writer)))

(defn write-gif-to-file
  [file gif]
  (with-open [file-stream (io/output-stream (files/sanitize-file file))
              stream (img/create-image-output-stream file-stream)]
    (write-gif-to-stream stream gif)))

(defn- remap-indexed-image-to-gct
  ^BufferedImage
  [^BufferedImage source ^IndexColorModel gct ^Integer transparent-color-index]
  (let [type (.getType source)]
    (if (is-indexed source)
      (let [^IndexColorModel src-color-model (.getColorModel source)
            src-n (.getMapSize src-color-model)
            dst-n (.getMapSize gct)

            src-to-dest (mapv
                         (fn [^long index]
                           (if (= transparent-color-index index)
                             transparent-color-index
                             (loop [src-color (.getRGB src-color-model (int index))
                                    i 0]
                               (cond
                                 (= i dst-n) (throw (ex-info "Source palette contains a color not present in the global palette table"
                                                             {:src-index index
                                                              :rgb (format "#%06X" src-color)}))
                                 (= src-color (.getRGB gct i)) i
                                 :else (recur src-color (inc i))))))
                         (range src-n))

            src-width (.getWidth source)
            src-height (.getHeight source)
            src-raster (.getRaster source)

            ;; Jave ImageIO's writer is ASS and will clear out the transparentColorIndex if the source ICM
            ;; doesn't contain a transparent pixel. So if a transparent pixel exists for this frame, we have
            ;; to create a bespoke version of the color model just for this frame with a transparent pixel.
            gct (if transparent-color-index
                  (let [reds (byte-array dst-n)
                        greens (byte-array dst-n)
                        blues (byte-array dst-n)]
                    (.getReds gct reds)
                    (.getGreens gct greens)
                    (.getBlues gct blues)
                    (IndexColorModel. 8 dst-n reds greens blues transparent-color-index))
                  gct)

            dst-img (BufferedImage. src-width src-height BufferedImage/TYPE_BYTE_INDEXED gct)
            dst-raster (.getRaster dst-img)
            src-idx (int-array src-width)
            dst-idx (byte-array src-width)]
        (dotimes [y src-height]
          (.getSamples src-raster 0 y src-width 1 0 src-idx)
          (dotimes [x src-width]
            (let [v (aget src-idx x)]
              (aset dst-idx x (unchecked-byte (nth src-to-dest v)))))
          (.setDataElements dst-raster 0 y src-width 1 dst-idx))
        dst-img)
      source)))

(defn- get-gct-transparency-index
  [^ImageReader reader]
  (loop [frame-count (.getNumImages reader true)
         i 0]
    (when-not (= i frame-count)
      (if-let [tci (let [frame-metadata (.getImageMetadata reader i)
                         ^Element root (.getAsTree frame-metadata (.getNativeMetadataFormatName frame-metadata))
                         gce (get-node root graphic-control-extension)]
                     (when (and gce
                                (zero? (.getLength (.getElementsByTagName root "LocalColorTable")))
                                (= (.getAttribute gce "transparentColorFlag") "TRUE"))
                       (when-let [tci (.getAttribute gce "transparentColorIndex")]
                         (Integer/parseInt tci))))]
        tci
        (recur frame-count (inc i))))))


(defn get-gif-from-stream
  [stream]
  (with-open [gif-iis (img/create-image-input-stream stream)]
    (let [reader (doto (get-reader)
                   (.setInput gif-iis))
          sm (.getStreamMetadata reader)
          fmt (.getNativeMetadataFormatName sm)
          root (.getAsTree sm fmt)
          lsd (get-node root logical-screen-descriptor)
          gct-node (get-node root "GlobalColorTable")
          background-index (some-> lsd (.getAttribute "backgroundColorIndex") not-empty Integer/parseInt)
          gct (when-let [nl (when gct-node
                              (.getElementsByTagName gct-node "ColorTableEntry"))]
                (let [n (.getLength nl)
                      reds (byte-array n)
                      greens (byte-array n)
                      blues (byte-array n)]
                  (dotimes [i n]
                    (let [^Element e (.item nl i)
                          index (Integer/parseInt (.getAttribute e "index"))]
                      (aset-byte reds   index (unchecked-byte (Integer/parseInt (.getAttribute e "red"))))
                      (aset-byte greens index (unchecked-byte (Integer/parseInt (.getAttribute e "green"))))
                      (aset-byte blues  index (unchecked-byte (Integer/parseInt (.getAttribute e "blue"))))))
                  (IndexColorModel. 8 n reds greens blues)))]
      (cond->
          {:width (some-> lsd (.getAttribute "logicalScreenWidth") not-empty Integer/parseInt)
           :height (some-> lsd (.getAttribute "logicalScreenHeight") not-empty Integer/parseInt)
           :scenes (let [gif-scenes (get-gif-frames-from-reader  reader)]
                     (if gct
                       (mapv (fn [{:keys [colortable-is-local frame transparent-color-index] :as scene}]
                               (if colortable-is-local
                                 scene
                                 ;; Java ImageIO creates a different IndexColorModel object for each frame even if that frame
                                 ;; uses the global color table.  Here we create a new frame that is identical to the old one
                                 ;; but uses the global color table's IndexColorModel instead of the one created by ImageIO.
                                 (assoc scene :frame (remap-indexed-image-to-gct frame gct transparent-color-index))))
                             gif-scenes)
                       gif-scenes))}
        (some? gct) (assoc :global-color-table gct)
        (some? background-index) (assoc :background-index background-index)))))

(defn get-gif-from-file
  [file]
  (with-open [stream (io/input-stream (files/sanitize-file file))]
    (get-gif-from-stream stream)))

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

(defn pixel-is-opaque
  ^Boolean
  [^Integer p]
  (= (bit-and (bit-shift-right p 24) 0xFF) 0xFF))

(defn quantize-image
  ^BufferedImage
  [^BufferedImage img]
  (let [width (.getWidth img)
        height (.getHeight img)
        total-pixels (* width height)
        ;; Integer array used to read lines from the image.
        pixel-line! (int-array width)

        ;; unique-colors: Hash set of unique colors, nor nil if the number of unique colors exceeds 256.
        ;; opaque-pixel-count: number of pixels that have no transparency.
        {:keys [unique-colors opaque-pixel-count]}
        (loop [y 0
               unique-colors #{}
               opaque-pixel-count 0]
          (if (= y height)
            {:opaque-pixel-count opaque-pixel-count :unique-colors unique-colors}
            (let [{:keys [unique-colors
                          ^long opaque-pixel-count]}
                  (reduce (fn [{:keys [unique-colors
                                       ^long opaque-pixel-count]} ^Integer pixel]
                            {:opaque-pixel-count (if (pixel-is-opaque pixel)
                                                   (inc opaque-pixel-count)
                                                   opaque-pixel-count)
                             :unique-colors (if (or (not unique-colors) (>= (count unique-colors) 256))
                                              nil
                                              (conj unique-colors pixel))})
                          {:unique-colors unique-colors
                           :opaque-pixel-count opaque-pixel-count}
                          (.getRGB img 0 y width 1 pixel-line! 0 width))]
              (recur (inc y) unique-colors opaque-pixel-count))))

        has-transparent-pixels (< opaque-pixel-count total-pixels)]
    (cond
      (= opaque-pixel-count 0)
      ;; Empty image:
      (let [empty-img (img/->buffered-image width height img/img-type-byte-index (img/ints-to-byte-icm (int-array [0]) 0))
            raster-buffer (-> empty-img
                              (img/get-raster)
                              (img/get-data-byte-buffer)
                              (img/get-byte-data))]
        (dotimes [i (alength raster-buffer)]
          (aset-byte raster-buffer i (byte 0)))
        empty-img)

      (and unique-colors (< (count unique-colors) (- 256 (if has-transparent-pixels 1 0))))
      ;; We don't need to bother quantizing if there's less than 256 colors anyway.
      (img/copy-img img (img/->buffered-image width height
                                              img/img-type-byte-index
                                              (img/ints-to-byte-icm (int-array (condj (vec unique-colors) (if has-transparent-pixels 0 nil)))
                                                                    (if has-transparent-pixels 0 nil))))

      :else
      ;; Quantize the image:
      (let [pixel->lab (fn [^Integer i]
                         ^doubles
                         ;; If the pixel is opaque then convert to lab.
                         ;; Otherwise, nil.
                         (if (pixel-is-opaque i)
                           (double-array (cs/srgb->cielab (/ (bit-and (bit-shift-right i 16) 0xFF) 255.0)
                                                          (/ (bit-and (bit-shift-right i 8) 0xFF) 255.0)
                                                          (/ (bit-and i 0xFF) 255.0)))
                           nil))
            max-unique-colors (if has-transparent-pixels
                                255
                                256)
            ;; A sample of lab-stream excluding transparent pixels.
            image-sample (into [] (comp
                                   (filter some?)
                                   (random-sample (min 1.0 (/ 200000.0 opaque-pixel-count)))
                                   (map (fn [^doubles d] (DoublePoint. d))))
                               ;; Pixels -> CIE lab doubles
                               (flatten (map (fn [^Integer y] (mapv pixel->lab (.getRGB img 0 y width 1 pixel-line! 0 width))) (range height))))

            distance-measurer (EuclideanDistance.)
            ;; Vector of double-arrays in CIELAB.  A quantized version image-sample.
            palette-lab (mapv (fn [^CentroidCluster centroid]
                                (.getPoint (.getCenter centroid)))
                              (.cluster (MultiKMeansPlusPlusClusterer.
                                         (KMeansPlusPlusClusterer.
                                          max-unique-colors
                                          10
                                          distance-measurer
                                          (MersenneTwister.))
                                         1)
                                        image-sample))

            palettized-image
            (img/->buffered-image
             width height BufferedImage/TYPE_BYTE_INDEXED
             (img/rgb-bytes-to-byte-icm
              (let [result-array (byte-array (* 3 256))]
                ;; CIELAB palette to array of RGB byte triplets.
                (dotimes [i (count palette-lab)]
                  (let [[l a b] (nth palette-lab i)
                        [r g b] (cs/cielab->srgb l a b)]
                    (aset-byte result-array (* i 3) (cs/normalized->byte r))
                    (aset-byte result-array (+ 1 (* i 3)) (cs/normalized->byte g))
                    (aset-byte result-array (+ 2 (* i 3)) (cs/normalized->byte b))))
                (when has-transparent-pixels
                  (aset-byte result-array (* 255 3) 0)
                  (aset-byte result-array (+ 1 (* 255 3)) 0)
                  (aset-byte result-array (+ 2 (* 255 3)) 0))
                result-array)
              (if has-transparent-pixels
                255
                nil)))

            raster-buffer (-> palettized-image
                              (img/get-raster)
                              (img/get-data-byte-buffer)
                              (img/get-byte-data))]
        (doseq [y (range height)]
          (.getRGB img 0 y width 1 pixel-line! 0 width)
          (dotimes [x width]
            (aset-byte raster-buffer (+ (* y width) x)
                       (unchecked-byte
                        (if-let [pixel (pixel->lab (aget pixel-line! x))]
                          (find-closest-palette-match-index distance-measurer pixel palette-lab)
                          255)))))
        palettized-image))))

(defn caption-gif
  [{:keys [width height scenes] :as gif} caption caption-ratio]
  (let [content-pixel-width width
        content-pixel-height height
        caption-pixel-height (ceilint (* content-pixel-height (/ caption-ratio 100.0)))

        final-image-pixel-height (+ caption-pixel-height content-pixel-height)

        ;; Calculate new frame offsets taking the caption area into account.
        gif-frames (mapv (fn [{:keys [x-offset y-offset] :as scene}]
                           (let [offset-img (-> (geom/get-translate-transform 0.0 caption-pixel-height)
                                                (geom/concatenate-transforms (geom/get-translate-transform x-offset y-offset)))]
                             (assoc scene
                                    :x-offset (roundint (geom/get-translate-x offset-img))
                                    :y-offset (roundint (geom/get-translate-y offset-img)))))
                         scenes)
        caption-frame (quantize-image
                       (let [temp-file (files/create-temp-file)]
                         (try
                           (img/write-img-to (:frame (first scenes)) temp-file)
                           (svg/render-document (caption/caption-svg content-pixel-width content-pixel-height temp-file caption caption-pixel-height)
                                                content-pixel-width final-image-pixel-height)
                           (finally
                             (files/delete-if-exists temp-file)))))]
    (assoc gif
           :scenes (cons {:frame caption-frame :x-offset 0 :y-offset 0 :colortable-is-local true
                          :duration (centiseconds 1) :disposal "doNotDispose"} gif-frames)
           :height final-image-pixel-height)))
