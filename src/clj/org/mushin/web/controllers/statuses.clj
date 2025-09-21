(ns org.mushin.web.controllers.statuses
  (:require [ring.util.http-response :refer [unauthorized! created ok not-found! accepted no-content]]
            [org.mushin.db.statuses :as db]
            [org.mushin.db.users :as user-db]
            [clojure.tools.logging :as log]
            [org.mushin.web.auth-utils :as auth-utils]
            [org.mushin.db.util :as db-u]
            [xtdb.api :as xt]
            [org.mushin.db.resources :as resources]
            [org.mushin.files :as files]
            [org.mushin.buffers :as buffers]
            [org.mushin.mime :as mime]
            [org.mushin.digest :as digest]
            [clojure.java.io :as io])
  (:import [java.nio.file Files]
           [java.io InputStream]
           [javax.imageio ImageIO ImageWriter ImageReader IIOImage ImageTypeSpecifier]
           [javax.imageio.metadata IIOMetadata IIOMetadataNode]
           [javax.imageio.stream ImageInputStream MemoryCacheImageInputStream ImageOutputStream]
           [java.awt.image BufferedImage RenderedImage IndexColorModel]
           [java.nio ByteOrder]))

(def create-picture-post-body
  [:map {:closed true}
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

(defn- verify-image-upload
  [^InputStream image-stream]
  true) ; TODO

(defn create-resource-from-static-img
  [^InputStream image-stream mime-type xtdb-node]
  (let [image-iis (ImageIO/createImageInputStream image-stream)
        img (ImageIO/read image-iis)
        _ (when-not img
            ;; For some reason, the above overload of imageio.read() closes the input stream...
            ;; unless an error occurred, in which case img is null.
            (.close image-iis)
            (throw (ex-info "Could not create image from image-stream" {:mime-type mime-type})))
        width (.getWidth img)
        height (.getHeight img)
        checksum (let [rgb-array (int-array width)
                       raw-byte-array (byte-array (* width 4))
                       md (digest/create-sha256-digest)
                       bb (buffers/set-byte-order (buffers/wrap-bytes raw-byte-array) ByteOrder/BIG_ENDIAN)]
                   ;; TODO add certain metadata support like EXIF orientation. Would need to rotate the image
                   ;; and add that information to the checksum.
                   (dotimes [y height]
                     (.getRGB img 0 y width 1 rgb-array 0 width)
                     (buffers/copy-ints-to-byte-buffer! rgb-array bb)
                     (digest/update-digest-buffer md raw-byte-array)
                     (buffers/clear-byte-buffer bb))
                   (digest/digest->b64 md))
        output-file-path (files/create-temp-file "" "")]
    (try
      (with-open [temp-output-file (io/output-stream (str output-file-path))
                  image-ios (ImageIO/createImageOutputStream temp-output-file)]
        (ImageIO/write img (mime/mime-types mime-type) image-ios))
      (resources/create-resource-from-file! xtdb-node output-file-path checksum mime-type)
      (catch Exception ex
        (throw ex))
      (finally
        (files/delete output-file-path)))))

(defn- create-resource-from-gif
  [^InputStream image-stream mime-type]
  nil)

(defn- create-resource-from-upload
  [^InputStream image-stream mime-type xtdb-node]
  (cond
    (= mime-type :gif) (create-resource-from-gif image-stream mime-type)
    (some #(= mime-type %) [:png :jpg]) (create-resource-from-static-img image-stream mime-type xtdb-node)
    :else (throw (ex-info "Mime type is not supported" {:mime-type mime-type}))))

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
(defn create-picture-post!
  [{:keys [xtdb-node]}
   {{{{:keys [image]} :status} :body} :parameters {:keys [user-id]} :session}]
  (when-not user-id
    (unauthorized! {:error :not-logged-in :message "You are not logged in, and so have no permissions to perform this action"}))
  (let [{:keys [tempfile filename]} image
        {:keys [xt/id]} (resources/create-resource-from-file! xtdb-node tempfile filename)]
;(db/create-status! xtdb-node {:image id :text "Text"} user-id)
    ))

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
  [{:keys [xtdb-node]}
   {{{{:keys [text media]} :content :keys [reply-to]} :body} :parameters {:keys [user-id]} :session :keys [mushin/async?] :as req}]
  (prn req)
  (when-not user-id
    (unauthorized! {:error :not-logged-in :message "You are not logged in, and so have no permissions to perform this action"}))
  (if media
    (create-media-status!)
    (create-text-status! xtdb-node user-id text reply-to async?)))
