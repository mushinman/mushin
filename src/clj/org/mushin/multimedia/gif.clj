(ns org.mushin.multimedia.gif
  (:require [clojure.java.io :as io]
            [java-time.api :as time]
            [org.mushin.geom :as geom]
            [org.mushin.with-disposable :refer [with-disposable]]
            [org.mushin.files :as files]
            [org.mushin.multimedia.svg :as svg]
            [org.mushin.multimedia.captions :as caption]
            [clojure.math :as math]
            [org.mushin.multimedia.colorspace :as cs]
            [org.mushin.multimedia.img :as img])
  (:import [javax.imageio ImageIO ImageWriter ImageReader IIOImage ImageTypeSpecifier]
           [javax.imageio.metadata IIOMetadata IIOMetadataNode]
           [org.apache.commons.math3.ml.clustering KMeansPlusPlusClusterer DoublePoint MultiKMeansPlusPlusClusterer CentroidCluster Clusterable]
           [org.apache.commons.math3.ml.distance EuclideanDistance]
           [org.apache.commons.math3.random MersenneTwister]
           [java.util Random]
           [java.time Duration]
           [javax.imageio.stream ImageInputStream MemoryCacheImageInputStream ImageOutputStream]
           [java.awt Transparency]
           [java.awt.image BufferedImage RenderedImage IndexColorModel]
           [java.awt.geom AffineTransform]
           [java.io Closeable]))

(defonce logical-screen-descriptor "LogicalScreenDescriptor")

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

;(defn create-)
;

(defn- write-color-model!
  ^IIOMetadataNode
  [^IndexColorModel icm ^IIOMetadataNode node]
  (dotimes [n (.getMapSize icm)]
    (.appendChild node
                  (doto (IIOMetadataNode. "ColorTableEntry")
                    (.setAttribute "index" (str n))
                    (.setAttribute "red" (str (.getRed icm (int n))))
                    (.setAttribute "green" (str (.getGreen icm (int n))))
                    (.setAttribute "blue" (str (.getBlue icm (int n)))))))
  node)

(defn get-gif-timeframes
  "Create a sequence of frame times for each frame in `gif`"
  [gif]
  (with-disposable [reader (get-reader)]
    (with-open [iis (ImageIO/createImageInputStream gif)]
      (.setInput reader iis)
      (mapv (fn [^long i]
              (let [frame-metadata (.getImageMetadata reader i)
                    root (.getAsTree frame-metadata (.getNativeMetadataFormatName frame-metadata))
                    gce (.item (.getElementsByTagName root "GraphicControlExtension") 0)]
                ;; Here we clamp to 1 ms because it's technically possible for a gif to have a frame time of 0ms.
                (centiseconds (max (Integer/parseInt (.getAttribute gce "delayTime")) 1))))
            (range (.getNumImages reader true))))))

(defn get-gif-frames-from-reader
  [^ImageReader reader]
  (mapv (fn [^long i]
          (let [frame-metadata (.getImageMetadata reader i)
                root (.getAsTree frame-metadata (.getNativeMetadataFormatName frame-metadata))
                gce (.item (.getElementsByTagName root "GraphicControlExtension") 0)]
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
  [^ImageWriter writer {:keys [duration frame disposal x-offset y-offset colortable-is-local transparent-color-index]}]
  (let [write-param (.getDefaultWriteParam writer)]
    (.writeToSequence writer (IIOImage. frame nil
                                        (let [metadata (.getDefaultImageMetadata writer
                                                                                 (ImageTypeSpecifier/createFromRenderedImage frame)
                                                                                 write-param)
                                              fmt (.getNativeMetadataFormatName metadata)
                                              root (.getAsTree metadata fmt)
                                              gce (.item (.getElementsByTagName root "GraphicControlExtension") 0)]
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
                                                  map-size (.getMapSize color-model)
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
      (let [src-color-model (.getColorModel source)
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
                         root (.getAsTree frame-metadata (.getNativeMetadataFormatName frame-metadata))
                         gce (.item (.getElementsByTagName root "GraphicControlExtension") 0)]
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
                    (let [e (.item nl i)
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

(defn add-scene
  [{:keys [scenes] :as gif} scene]
  (assoc gif :scenes (cons scene scenes)))

(defn get-first-frame
  [{:keys [scenes]}]
  (:frame (first scenes)))

(defn apply-caption-to-frame
  [caption-img caption-stop-y frame]
  (let [combined-img (BufferedImage. (.getWidth caption-img) (.getHeight caption-img) BufferedImage/TYPE_INT_ARGB)]
    (doto (.getGraphics combined-img)
      (.drawRenderedImage caption-img (AffineTransform.))
      (.drawRenderedImage frame (AffineTransform/getTranslateInstance 0 caption-stop-y))
      (.dispose))
    combined-img))

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
                               (random-sample (min 1.0 (/ 200000.0 opaque-pixel-count))) ; TODO if the image is entirely tansparent or empty that will result in a div by 0.
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
                           (svg/render-document (caption/make-meme-svg content-pixel-width content-pixel-height temp-file caption caption-pixel-height)
                                                content-pixel-width final-image-pixel-height)
                           (finally
                             (files/delete-if-exists temp-file)))))]
    (assoc gif
           :scenes (cons {:frame caption-frame :x-offset 0 :y-offset 0 :colortable-is-local true
                          :duration (centiseconds 1) :disposal "doNotDispose"} gif-frames)
           :height final-image-pixel-height)))
