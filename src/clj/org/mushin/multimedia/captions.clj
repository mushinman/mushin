(ns org.mushin.multimedia.captions
  (:require [org.mushin.multimedia.svg :as svg])
  (:import [org.w3c.dom.svg SVGDocument]))

(defn caption-svg
  ^SVGDocument
  [image-pixel-width image-pixel-height content-href caption caption-pixel-height]

  (when (or (nil? image-pixel-width) (nil? image-pixel-height) (<= (double image-pixel-width) 0.0))
    (throw (ex-info "image width/height must be positive" {:pxw image-pixel-width :pxh image-pixel-height})))

  (let [virtual-width 1000.0
        img-virtual-height (* virtual-width (/ (double image-pixel-height) (double image-pixel-width)))
        caption-virtual-height (* img-virtual-height (/ (double caption-pixel-height) (double image-pixel-height)))
        total-virtual-height (+ caption-virtual-height img-virtual-height)]
    (svg/hiccough->svg
     [:svg {:view-box (format "0 0 1000 %.0f" total-virtual-height)
            :width "100%"
            :preserve-aspect-ratio "xMidYMid meet"}
      [:style
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
image { image-rendering: auto; }"]
      ;(.setAttributeNS mushin-el "http://www.w3.org/2000/xmlns/" "xmlns:mushin" "urn:mushin")
      [:metadata
       {:urn:mushin/xmlns:mushin "http://www.w3.org/2000/xmlns/"}
       [:urn:mushin/mushin:content-image#mushin-metadata
        {:urn:musin/mushin:pxw (long image-pixel-width)
         :urn:musin/mushin:pxh (long image-pixel-height)}]]
      [:rect#frame
       {:x 0
        :y 0
        :width 1000
        :height caption-virtual-height}]
      [:image
       {:x 0
        :y caption-virtual-height
        :width "100%"
        :height (format "%.0f" img-virtual-height)}
       content-href]

      [:text#caption
       {:x 500
        :y 90}
       caption]])))
