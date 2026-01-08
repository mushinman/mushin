(ns org.mushin.db.media
  (:require [org.mushin.mime :as mime]
            [org.mushin.files :as files]
            [org.mushin.codecs :as codecs]
            [org.mushin.multimedia.gif :as gif]
            [org.mushin.multimedia.svg :as svg]
            [org.mushin.multimedia.captions :as caption]
            [org.mushin.digest :as digest]
            [clojure.java.io :as io]
            [org.mushin.multimedia.img :as img]
            [org.mushin.resources.resource-map :as res])
  (:import [java.awt.image BufferedImage]
           [java.io InputStream]
           [javax.imageio ImageIO]))


(defn create-resource-from-buffered-image!
  "Create and store a buffered image as a resource.

  # Arguments
  - `img`: The image to store.
  - `mime-type`: The MIME type of the resulting stored image.
  - `resource-map`: The resource provider to store the image.

  # Return
  A URI to the stored image. The resource name will be based off the SHA-256 sum
  of the image's sRGB content."
  [^BufferedImage img mime-type resource-map]
  (let [mime-ext (mime/mime-type-to-extensions mime-type)
        resource-name (str (codecs/bytes->b64u (img/checksum-image img)) "." mime-ext)]
    (if-let [metadata (res/metadata resource-map resource-name)]
      metadata
      (let [output-file-path (files/create-temp-file)]
        (try
          (with-open [temp-output-file (io/output-stream (str output-file-path))
                      image-ios (ImageIO/createImageOutputStream temp-output-file)]
            (img/write-img-from-mime-type img mime-type image-ios))
          (res/create! resource-map resource-name output-file-path mime-type)
          (finally
            (files/delete-if-exists output-file-path)))))))

(defn create-resource-from-static-image!

  "Create and store an image as a resource.

  # Arguments
  - `img`: The image to store.
  - `mime-type`: The MIME type of the resulting stored image.
  - `resource-map`: The resource provider to store the image.

  # Return
  A URI to the stored image. The resource name will be based off the SHA-256 sum
  of the image's sRGB content."
  [img-path mime-type resource-map]
  (create-resource-from-buffered-image! (img/->buffered-image (str img-path))
                                        mime-type
                                        resource-map))

(defn create-captioned-resource-from-buffered-image!
  [^BufferedImage img resource-map mime-type text ratio]
  (let [mime-ext (mime/mime-type-to-extensions mime-type)
        resource-name (str (codecs/bytes->b64u (img/checksum-image img)) "." mime-ext)
        output-file-path (files/create-temp-file)]
    (try
      ;; Unlike in the non-captioned variant we unconditionally create the temp image
      ;; since we'll need it to render the captioned image.
      (with-open [temp-output-file (io/output-stream (str output-file-path))
                  image-ios (ImageIO/createImageOutputStream temp-output-file)]
        (img/write-img-from-mime-type img mime-type image-ios))

      (if-let [metadata (res/metadata resource-map resource-name)]
        metadata
        (res/create! resource-map resource-name output-file-path mime-type))

      (let [;; Save the captioned version of the image, the SVG.
            width (.getWidth img)
            height (.getHeight img)
            caption-pixel-height (* height (/ ratio 100.0))
            full-img-height (+ caption-pixel-height height)
            rendered-caption-img-name (create-resource-from-buffered-image!
                                       (svg/render-document
                                        (caption/caption-svg width height
                                                       (files/path->uri output-file-path) text caption-pixel-height)
                                        width full-img-height)
                                       mime-type resource-map)

            caption-svg-name
            (let [temp-svg (files/create-temp-file)]
              (try
                (svg/write-svgdoc-to-file! (caption/caption-svg width height
                                                          (res/to-uri resource-map resource-name) text caption-pixel-height)
                                           temp-svg)
                (let [resource-name (str (digest/digest->b64u (digest/digest-file temp-svg)) ".svg")]
                  (res/create! resource-map resource-name temp-svg mime-type))
                (finally
                  (files/delete-if-exists temp-svg))))]
        {:base-img resource-name :captioned-img rendered-caption-img-name :svg-doc caption-svg-name})
      (finally
        (files/delete-if-exists output-file-path)))))

(defn create-captioned-resource-from-static-image!

  "Create and store an image as a resource.

  # Arguments
  - `img`: The image to store.
  - `resource-map`: The resource provider to store the image.
  - `mime-type`: The MIME type of the resulting stored image.

  # Return
  A URI to the stored image. The resource name will be based off the SHA-256 sum
  of the image's sRGB content."
  [img-path resource-map mime-type text ratio]
  (create-resource-from-buffered-image! (img/->buffered-image (str img-path))
                                        mime-type
                                        resource-map))

(defn create-resource-for-gif!
  [^InputStream gif-stream resource-map]
  (let [gif (gif/get-gif-from-stream gif-stream)
        resource-name (str (loop [i 0
                                  md (digest/create-sha256-digest)
                                  scenes (:scenes gif)]
                             ;; Digest every frame.
                             ;; TODO similar to the static frames we probably want to add some metadata
                             ;; to the checksum, like if it loops or not.
                             (if-not (= i (count gif))
                               (do (img/digest-img! (:frame (nth scenes i)) md)
                                   (recur (inc i) md scenes))
                               (digest/digest->b64u md)))
                           ".gif")]
    (if-let [metadata (res/metadata resource-map resource-name)]
      metadata
      (let [output-file-path (files/create-temp-file "" "")]
        (try
          (with-open [temp-output-file (io/output-stream (str output-file-path))
                      image-ios (ImageIO/createImageOutputStream temp-output-file)]
            (gif/write-gif-to-stream image-ios gif))
          (res/create! resource-map resource-name output-file-path "image/gif")
          (finally
            (files/delete-if-exists output-file-path)))))))
