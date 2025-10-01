(ns org.mushin.web.controllers.statuses
  (:require [ring.util.http-response :refer [unauthorized! created ok not-found! accepted no-content bad-request!]]
            [org.mushin.db.statuses :as db]
            [org.mushin.db.users :as user-db]
            [clojure.tools.logging :as log]
            [org.mushin.web.auth-utils :as auth-utils]
            [org.mushin.db.util :as db-u]
            [xtdb.api :as xt]
            [org.mushin.files :as files]
            [org.mushin.resources.resource-map :as res]
            [lambdaisland.uri :refer [uri join]]
            [org.mushin.multimedia.svg :as svg]
            [org.mushin.mime :as mime]
            [org.mushin.multimedia.img :as img]
            [clojure.java.io :as io]
            [org.mushin.digest :as digest]
            [org.mushin.codecs :as codecs]
            [org.mushin.multimedia.gif :as gif])
  (:import [java.nio.file Files]
           [java.io InputStream]
           [javax.imageio ImageIO ImageWriter ImageReader IIOImage ImageTypeSpecifier]
           [javax.imageio.metadata IIOMetadata IIOMetadataNode]
           [io.sf.carte.echosvg.anim.dom SVGDOMImplementation]
           [javax.imageio.stream ImageInputStream MemoryCacheImageInputStream ImageOutputStream]
           [org.w3c.dom.svg SVGDocument SVGFitToViewBox]
           [java.awt.image BufferedImage RenderedImage IndexColorModel]
           [java.nio ByteOrder]))

(def create-picture-post-body
  [:map
   [:image {:description "mulitpart file"}]
   [:text :string]])

(def create-status-body
  [:map {:closed true}
   [:reply-to {:optional true} :uuid]
   [:content                   [:map {:closed true}
                                [:text {:optional true} :string]
                                [:media  {:description "mulitpart file" :optional true} :any]]]])

(def create-text-post-body
  [:map
   [:text                      :string]
   [:reply-to {:optional true} :uuid]])

(def get-timeline-query
  [:map {:closed true}
   [:reverse {:optional true}   :boolean]
   [:limit   {:optional true}   :int]
   [:offset  {:optional true}   :int]])

(def get-status-query
  [:map {:closed true}
   [:get-comments {:optional true} :boolean]])

(def status-query
  [:map [:id :uuid]])

(def caption
  [:map
   [:type    :caption]
   [:text    [:string {:min 0 :max 500}]]
   [:ratio   [:int {:min 10 :max 70}]]])

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
#testarea {
  fill: #F00;
}
image { image-rendering: auto; }")

(defn- make-meme-svg
  "Builds an SVG Document."
  ^SVGDocument
  [image-pixel-width image-pixel-height content-href caption caption-pixel-height]
  (when (or (nil? image-pixel-width) (nil? image-pixel-height) (<= (double image-pixel-width) 0.0))
    (throw (ex-info "image width/height must be positive" {:pxw image-pixel-width :pxh image-pixel-height})))

  (let [impl (SVGDOMImplementation/getDOMImplementation)
        doc  (.createDocument impl svg/svg-ns "svg" nil)
        root (.getDocumentElement doc)
        virtual-width 1000.0
        img-height (* virtual-width (/ (double image-pixel-height) (double image-pixel-width)))
        caption-virtual-height (* virtual-width (/ (double caption-pixel-height) (double image-pixel-height)))
        total-height (+ caption-virtual-height img-height)]

    (doto root
      (.setAttribute "viewBox" (format "0 0 1000 %.0f" total-height))
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
        (.setAttribute "height" (format "%.0f" img-height))
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


(defn- verify-image-upload
  [^InputStream image-stream]
  true) ; TODO

(defn- create-resource-from-static-img!
  [^BufferedImage img resource-map mime-type]
  (let [mime-ext (mime/mime-type-to-extensions mime-type)
        resource-name (str (codecs/bytes->b64u (img/checksum-image img)) "." mime-ext)]
    (if (res/exists? resource-map resource-name)
      resource-name
      (let [output-file-path (files/create-temp-file "" "")]
        (try
          (with-open [temp-output-file (io/output-stream (str output-file-path))
                      image-ios (ImageIO/createImageOutputStream temp-output-file)]
            (img/write-img-from-mime-type img mime-type image-ios))
          (res/create! resource-map resource-name output-file-path)
          (catch Exception ex
            (throw ex))
          (finally
            (files/delete-if-exists output-file-path)))))))

(defn- create-captioned-static-img-resource!
  [^BufferedImage img resource-map mime-type text ratio]
  (let [mime-ext (mime/mime-type-to-extensions mime-type)
        resource-name (str (codecs/bytes->b64u (img/checksum-image img)) "." mime-ext)
        output-file-path (files/create-temp-file "" "")]
    (try
      ;; Unlike in the non-captioned variant we unconditionally create the temp image
      ;; since we'll need it to render the captioned image.
      (with-open [temp-output-file (io/output-stream (str output-file-path))
                  image-ios (ImageIO/createImageOutputStream temp-output-file)]
        (img/write-img-from-mime-type img mime-type image-ios))

      (if (res/exists? resource-map resource-name)
        ;; Ensure resource exists.
        resource-name
        (res/create! resource-map resource-name output-file-path))

      (let [width (.getWidth img)
            height (.getHeight img)
            caption-pixel-height (* height (/ ratio 100.0))
            full-img-height (+ caption-pixel-height height)
            rendered-caption-img-name (create-resource-from-static-img!
                                       (svg/render-document
                                        (make-meme-svg width height
                                                       (files/path->uri output-file-path) text caption-pixel-height) width full-img-height)
                                       resource-map mime-type)
            ]
        )
      {:base-img resource-name :captioned-img nil :svg-doc :nil}
      (catch Exception ex
        (throw ex))
      (finally
        (files/delete-if-exists output-file-path)))))

(defn create-resource-for-gif!
  [^InputStream gif-stream resource-map]
  (let [gif (gif/get-gif-from-stream gif-stream) ;; TODO catch exceptions and rethrow.
        resource-name (str (loop [i 0
                                  md (digest/create-sha256-digest)
                                  scenes (:scenes gif)]
                             ;; Digest every frame.
                             ;; TODO similar to the static frames we probably want to add some metadata
                             ;; to the checksum, like if it loops or not.
                             (if-not (= i (count gif))
                               (do (img/digest-img! (:frame (nth scenes i)) md)
                                   (recur (inc i) md scenes))
                               (codecs/bytes->b64u (digest/digest->bytes md))))
                           ".gif")]
    (if (res/exists? resource-map resource-name)
      resource-name
      (let [output-file-path (files/create-temp-file "" "")]
        (try
          (with-open [temp-output-file (io/output-stream (str output-file-path))
                      image-ios (ImageIO/createImageOutputStream temp-output-file)]
            (gif/write-gif-to image-ios gif))
          (res/create! resource-map resource-name output-file-path)
          (catch Exception ex
            (throw ex))
          (finally
            (files/delete-if-exists output-file-path)))))))



(defn create-resource-from-uploaded-file!
  [file-location resource-map]
  (let [file-location-path (files/path file-location)
        file-mime-type (files/probe-content-type file-location-path)]
    (cond
      (some #(= % file-mime-type) ["image/png" "image/jpeg"])
      (if-let [img (ImageIO/read (io/file file-location))]
        (create-resource-from-static-img! img resource-map file-mime-type)
        ;; I think img being nil means the image was invalid?
        (bad-request! {:error :invalid-image-type :mime-type file-mime-type}))

      (= file-mime-type "image/gif")
      (with-open [gif-is (io/input-stream file-location)]
        (create-resource-for-gif! gif-is resource-map))

      :else
      (bad-request! {:error :invalid-image-type :mime-type file-mime-type}))))

;; (defn create-captioned-img-resource
;;   [^InputStream image-stream mime-type xtdb-node]
;;   (let [image-iis (ImageIO/createImageInputStream image-stream)
;;         img (ImageIO/read image-iis)
;;         _ (when-not img
;;             ;; For some reason, the above overload of imageio.read() closes the input stream...
;;             ;; unless an error occurred, in which case img is null.
;;             (.close image-iis)
;;             (throw (ex-info "Could not create image from image-stream" {:mime-type mime-type})))
;;         resource-name (let [checksum (img/checksum-image img)]
;;                         (if-let [resource (resources/get-resource xtdb-node checksum)]
;;                           resource
;;                           checksum))
;;         output-file-path (files/create-temp-file "" "")]
;;     (try
;;       (with-open [temp-output-file (io/output-stream (str output-file-path))
;;                   image-ios (ImageIO/createImageOutputStream temp-output-file)]
;;         (ImageIO/write img (mime/mime-types mime-type) image-ios))
;;       (resources/create-resource-from-file! xtdb-node output-file-path resource-name mime-type)

;;       ;; We need to create a resource for the image with the caption.
;;       ;; We render with a reference to the temporary file since it's
;;       ;; easier to set up than rendering with the image's final location.
;;       (let [img-width (.getWidth img)
;;             img-height (.getHeight img)
;;             captioned-img (svg/render-document (svg/make-meme-svg img-width img-height (files/path->uri output-file-path) "Hello world" 150.0) img-width (+ img-height 150))
;;             resource-name (let [checksum (img/checksum-image captioned-img)]
;;                             (if-let [resource (resources/get-resource xtdb-node checksum)]
;;                               resource
;;                               checksum))
;;             ;; TODO i have to figure out how to build the URI to the resource....
;;             ])
;;       (catch Exception ex
;;         (throw ex))
;;       (finally
;;         (files/delete-if-exists output-file-path)))))

(defn- create-resource-from-gif
  [^InputStream image-stream mime-type]
  nil)


(defn get-timeline
  [{:keys [xtdb-node]}
   {{:keys [nickname]} :path-params {:keys [user-id]} :session :keys [query-params]}]
  ;; TODO check if the sessioned user is able to see this post.
  (let [user-id (or (user-db/get-user-id-by-nickname xtdb-node nickname)
                    (not-found! {:error :user-not-found :message "A user by that nickname was not found"}))
        db-offset (or (:offset query-params) 0)
        db-limit  (or (:limit query-params) 64)
        direction (if (or (:reverse query-params) false)
                    :desc
                    :asc)
        statuses (xt/q xtdb-node (xt/template (-> (from :mushin.db/statuses [tags created-at updated-at content xt/id {:user ~user-id}])
                                                  (order-by {:val created-at, :dir ~direction})
                                                  (offset ~db-offset)
                                                  (limit ~db-limit))))]
    (ok {:statuses statuses :user nickname :offset (+ db-offset (count statuses))})))

(defn get-status
  [{:keys [xtdb-node]}
   {{{:keys [id]} :path {:keys [get-comments]} :query} :parameters}]
  (log/info "And " get-comments)
  (if-let [status (db/get-status-by-id xtdb-node id)]
    (ok (-> {:status status}
            (merge (when-let [comments (when get-comments
                                         (or (not-empty (db/get-comments-for-status xtdb-node '[user content xt/id] id)) []))]
                     {:comments comments}))))
    (not-found! {:error :status-not-found :message (str "A status with the id " id " was not found")})))

(defn delete-status!
  [{:keys [xtdb-node]}
   {{{:keys [id]} :path} :parameters {:keys [user-id]} :session :keys [mushin/async?] :as req}]
  (let [session-user user-id
        {:keys [post-owner xt/id]} (db/get-status-by-id xtdb-node '[user xt/id] id)]
    (when-not id
      (not-found! {:error :post-not-found :message "The post you were trying to delete was not found"}))
    (log/info {:event :delete-status :user user-id :status-owner post-owner})
    (auth-utils/user-has-permissions-for! session-user post-owner)

    (if async?
      (do
        (xt/submit-tx xtdb-node [[:delete-docs :mushin.db/statuses id]])
        (accepted {:message "The post has been queued for deletion"}))
      (do
        (xt/execute-tx xtdb-node [[:delete-docs :mushin.db/statuses id]])
        (no-content)))))

;; TODO this won't work until I combine the POST request for all the content types into a single call.
(defn put-status!
  [{:keys [xtdb-node]}
   {{{:keys [id]} :path {:keys [content]} :body} :parameters {:keys [user-id]} :session}]
  (let [session-user user-id
        {:keys [post-owner xt/id]} (db/get-status-by-id xtdb-node '[user xt/id] id)]
    (when-not post-owner
      (not-found! {:error :post-not-found :message "The post you were trying to delete was not found"})
      (log/info {:event :edit-status :user user-id :status-owner post-owner :content content})
      (auth-utils/user-has-permissions-for! session-user post-owner)
      (xt/submit-tx xtdb-node [[:delete-docs :mushin.db/statuses id]]))))

(defn create-text-post!
  [{:keys [xtdb-node]}
   {{{:keys [text reply-to]} :body} :parameters {:keys [user-id]} :session :as req}]
  (when-not user-id
    (unauthorized! {:error :not-logged-in :message "You are not logged in, and so have no permissions to perform this action"}))
  )

;; TODO come back to this.

(defn create-media-status!
  [xtdb-node user-id {:keys [tempfile filename]} text reply-to async?]
  )

(defn create-text-status!
  [xtdb-node user-id text reply-to async?]
  (let [{:keys [xt/id] :as new-status} (db/create-status user-id {:text text} {:reply-to reply-to})]
    (log/info "Creating text status" {:event :created-status :status-type :text :user-id user-id :content {:text text} :reply-to reply-to})
    (if async?
      (do
        (db-u/submit-tx xtdb-node [[:put-docs :mushin.db/statuses new-status]])
        (accepted {:message "Your post has been queued for creation"}))
      (do
        (db-u/execute-tx  xtdb-node [[:put-docs :mushin.db/statuses new-status]])
        (created (str "/statuses/" id) {:status-id id})))))

;; TODO
(defn create-status-post!
  [{:keys [xtdb-node resource-map]}
   {{{{:keys [text media]} :content :keys [reply-to]} :body} :parameters {:keys [user-id]} :session :keys [mushin/async?] :as req}]
  (prn req)
  (when-not user-id
    (unauthorized! {:error :not-logged-in :message "You are not logged in, and so have no permissions to perform this action"}))
  (if media
    (create-media-status!)
    (create-text-status! xtdb-node user-id text reply-to async?)))
