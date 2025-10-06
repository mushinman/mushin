(ns org.mushin.multimedia.captions
  (:require [org.mushin.multimedia.svg :as svg])
  (:import [org.w3c.dom.svg SVGDocument]
           [io.sf.carte.echosvg.anim.dom SVGDOMImplementation]))

(def ^String meme-css
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
image { image-rendering: auto; }")

(defn make-meme-svg
  "Builds an SVG Document."
  ^SVGDocument
  [image-pixel-width image-pixel-height content-href caption caption-pixel-height]
  (when (or (nil? image-pixel-width) (nil? image-pixel-height) (<= (double image-pixel-width) 0.0))
    (throw (ex-info "image width/height must be positive" {:pxw image-pixel-width :pxh image-pixel-height})))

  (let [impl (SVGDOMImplementation/getDOMImplementation)
        doc  (.createDocument impl svg/svg-ns "svg" nil)
        root (.getDocumentElement doc)
        virtual-width 1000.0
        img-virtual-height (* virtual-width (/ (double image-pixel-height) (double image-pixel-width)))
        caption-virtual-height (* img-virtual-height (/ (double caption-pixel-height) (double image-pixel-height)))
        total-virtual-height (+ caption-virtual-height img-virtual-height)]

    (doto root
      (.setAttribute "viewBox" (format "0 0 1000 %.0f" total-virtual-height))
      (.setAttribute "width" "100%")
      (.setAttribute "preserveAspectRatio" "xMidYMid meet"))

    (let [style (.createElementNS doc svg/svg-ns "style")]
      (.appendChild style (.createTextNode doc meme-css))
      (.appendChild root style))

    ;; Custom mushin metadata
    (let [md        (.createElementNS doc svg/svg-ns "metadata")
          mushin-el (.createElementNS doc "urn:mushin" "mushin:content-image")]
      (.setAttributeNS mushin-el "http://www.w3.org/2000/xmlns/" "xmlns:mushin" "urn:mushin")
      (.setAttribute mushin-el "id" "mushin-metadata")
      (.setAttributeNS mushin-el "urn:mushin" "mushin:pxw" (str (long image-pixel-width)))
      (.setAttributeNS mushin-el "urn:mushin" "mushin:pxh" (str (long image-pixel-height)))
      (.appendChild md mushin-el)
      (.appendChild root md))


    (let [frame (.createElementNS doc svg/svg-ns "rect")]
      (doto frame
        (.setAttribute "id" "frame")
        (.setAttribute "x" "0")
        (.setAttribute "y" "0")
        (.setAttribute "width" "1000")
        (.setAttribute "height" (str caption-virtual-height)))
      (.appendChild root frame))

    (let [img (.createElementNS doc svg/svg-ns "image")]
      (doto img
        (.setAttribute "x" "0")
        (.setAttribute "y" (str caption-virtual-height))
        (.setAttribute "width" "100%")
        (.setAttribute "height" (format "%.0f" img-virtual-height))
        (.setAttribute "href" (str content-href)))
      (.appendChild root img))

    (let [txt (.createElementNS doc svg/svg-ns "text")]
      (doto txt
        (.setAttribute "id" "caption")
        (.setAttribute "x" "500")
        (.setAttribute "y" "90"))
      (.appendChild txt (.createTextNode doc (str caption)))
      (.appendChild root txt))
    doc))
