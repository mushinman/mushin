(ns org.mushin.multimedia.buffered-image-transcoder
  (:import [io.sf.carte.echosvg.transcoder TranscoderOutput]
           [java.awt.image BufferedImage])
  (:gen-class
   :name org.mushin.multimedia.BufferedImageTranscoder
   :extends io.sf.carte.echosvg.transcoder.image.ImageTranscoder
   :prefix "bit-"
   :init init
   :constructors {[] []}
   :state state
   :methods [[getImage [] java.awt.image.BufferedImage]]))


(defn bit-init
  []
  [[] (atom {})])

(defn- set-field!
  [this key value]
  (swap! (.state this) into {key value}))

(defn- get-field
  [this key]
  (@(.state this) key))

(defn bit-createImage
  ^BufferedImage
  [this ^Integer w ^Integer h]
  (let [img (BufferedImage. w h BufferedImage/TYPE_INT_ARGB)]
    (set-field! this :img img)
    img))

(defn bit-getImage
  ^BufferedImage
  [this]
  (get-field this :img))


(defn bit-writeImage
  [this ^BufferedImage img ^TranscoderOutput out])
