(ns org.mushin.web.controllers.statuses
  (:require [ring.util.http-response :refer [unauthorized! created ok not-found! accepted no-content bad-request!]]
            [org.mushin.db.statuses :as db]
            [org.mushin.db.users :as user-db]
            [clojure.tools.logging :as log]
            [org.mushin.web.auth-utils :as auth-utils]
            [org.mushin.db.util :as db-u]
            [xtdb.api :as xt]
            [org.mushin.files :as files]
            [clojure.walk :as walk]
            [clojure.java.io :as io])
  (:import [java.io InputStream]
           [javax.imageio ImageIO]))

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

(def layer-base
  [:map
   [:x :number]
   [:y :number]
   [:scale :number]
   [:rotation :number]])

(def comic
  [:map
   ;; Map of resources/uploaded files. Each resource cannot be identified
   ;; by its name directly since ring won't preserve the file name, so we
   ;; have to go by its resource name which is a SHA256 sum which is the value
   ;; in this map. The key is an alias used to reference the resource.
   [:resource-map
    [:map-of
     :keyword :string]]

   [:pages
    [:vector
     ;; Text or image or effect.
     [:merge layer-base
      [:map [:panel [:enum :text]]
       [:content :string]
       [:font-size pos-int?]]]

     [:merge layer-base
      [:map [:panel [:enum :img]]
       [:resource :string]]]

     [:merge layer-base
      [:map [:panel [:enum :effect]]
       [:content [:enum :brush :pencil :splatter]]]]]]])

;; (defn- comic-tag?
;;   [node]
;;   (and (vector? node) (keyword? (first node))))
;; ;; TODO finish
;; (defn- comic-dsl->hiccough
;;   [resource-map comic]
;;   (walk/postwalk
;;    (fn [node]
;;     (cond
;;       (comic-tag? node)
;;       ()

;;       :else node))
;;    comic))

;; (defn- verify-image-upload
;;   [^InputStream image-stream]
;;   true) ; TODO




;; (defn create-captioned-resources-from-uploaded-file!
;;   [file-location resource-map {:keys [text ratio]}]
;;   (let [file-location-path (files/path file-location)
;;         file-mime-type (files/probe-content-type file-location-path)]
;;     (cond
;;       (some #(= % file-mime-type) ["image/png" "image/jpeg"])
;;       (if-let [img (ImageIO/read (io/file file-location))]
;;         (create-captioned-static-img-resource! img resource-map file-mime-type text ratio)
;;         ;; I think img being nil means the image was invalid?
;;         (bad-request! {:error :invalid-image-type :mime-type file-mime-type}))

;;       (= file-mime-type "image/gif")
;;       () ; TODO
;;       :else
;;       (bad-request! {:error :invalid-image-type :mime-type file-mime-type}))))

;; (defn create-resource-from-uploaded-file!
;;   [file-location resource-map]
;;   (let [file-location-path (files/path file-location)
;;         file-mime-type (files/probe-content-type file-location-path)]
;;     (cond
;;       (some #(= % file-mime-type) ["image/png" "image/jpeg"])
;;       (if-let [img (ImageIO/read (io/file file-location))]
;;         (create-resource-from-static-img! img resource-map file-mime-type)
;;         ;; I think img being nil means the image was invalid?
;;         (bad-request! {:error :invalid-image-type :mime-type file-mime-type}))

;;       (= file-mime-type "image/gif")
;;       (with-open [gif-is (io/input-stream file-location)]
;;         (create-resource-for-gif! gif-is resource-map))

;;       :else
;;       (bad-request! {:error :invalid-image-type :mime-type file-mime-type}))))


;; (defn- create-resource-from-gif
;;   [^InputStream image-stream mime-type]
;;   nil)


;; (defn get-timeline
;;   [{:keys [xtdb-node]}
;;    {{:keys [nickname]} :path-params {:keys [user-id]} :session :keys [query-params]}]
;;   ;; TODO check if the sessioned user is able to see this post.
;;   (let [user-id (or (user-db/get-user-id-by-nickname xtdb-node nickname)
;;                     (not-found! {:error :user-not-found :message "A user by that nickname was not found"}))
;;         db-offset (or (:offset query-params) 0)
;;         db-limit  (or (:limit query-params) 64)
;;         direction (if (or (:reverse query-params) false)
;;                     :desc
;;                     :asc)
;;         statuses (xt/q xtdb-node (xt/template (-> (from :mushin.db/statuses [tags created-at updated-at content xt/id {:user ~user-id}])
;;                                                   (order-by {:val created-at, :dir ~direction})
;;                                                   (offset ~db-offset)
;;                                                   (limit ~db-limit))))]
;;     (ok {:statuses statuses :user nickname :offset (+ db-offset (count statuses))})))

;; (defn get-status
;;   [{:keys [xtdb-node]}
;;    {{{:keys [id]} :path {:keys [get-comments]} :query} :parameters}]
;;   (log/info "And " get-comments)
;;   (if-let [status (db/get-status-by-id xtdb-node id)]
;;     (ok (-> {:status status}
;;             (merge (when-let [comments (when get-comments
;;                                          (or (not-empty (db/get-comments-for-status xtdb-node '[user content xt/id] id)) []))]
;;                      {:comments comments}))))
;;     (not-found! {:error :status-not-found :message (str "A status with the id " id " was not found")})))

;; (defn delete-status!
;;   [{:keys [xtdb-node]}
;;    {{{:keys [id]} :path} :parameters {:keys [user-id]} :session :keys [mushin/async?] :as req}]
;;   (let [session-user user-id
;;         {:keys [post-owner xt/id]} (db/get-status-by-id xtdb-node '[user xt/id] id)]
;;     (when-not id
;;       (not-found! {:error :post-not-found :message "The post you were trying to delete was not found"}))
;;     (log/info {:event :delete-status :user user-id :status-owner post-owner})
;;     (auth-utils/user-has-permissions-for! session-user post-owner)

;;     (if async?
;;       (do
;;         (xt/submit-tx xtdb-node [[:delete-docs :mushin.db/statuses id]])
;;         (accepted {:message "The post has been queued for deletion"}))
;;       (do
;;         (xt/execute-tx xtdb-node [[:delete-docs :mushin.db/statuses id]])
;;         (no-content)))))

;; ;; TODO this won't work until I combine the POST request for all the content types into a single call.
;; (defn put-status!
;;   [{:keys [xtdb-node]}
;;    {{{:keys [id]} :path {:keys [content]} :body} :parameters {:keys [user-id]} :session}]
;;   (let [session-user user-id
;;         {:keys [post-owner xt/id]} (db/get-status-by-id xtdb-node '[user xt/id] id)]
;;     (when-not post-owner
;;       (not-found! {:error :post-not-found :message "The post you were trying to delete was not found"})
;;       (log/info {:event :edit-status :user user-id :status-owner post-owner :content content})
;;       (auth-utils/user-has-permissions-for! session-user post-owner)
;;       (xt/submit-tx xtdb-node [[:delete-docs :mushin.db/statuses id]]))))

;; (defn create-text-post!
;;   [{:keys [xtdb-node]}
;;    {{{:keys [text reply-to]} :body} :parameters {:keys [user-id]} :session :as req}]
;;   (when-not user-id
;;     (unauthorized! {:error :not-logged-in :message "You are not logged in, and so have no permissions to perform this action"}))
;;   )

;; ;; TODO come back to this.

;; (defn create-media-status!
;;   [xtdb-node user-id {:keys [tempfile filename]} text reply-to async?]
;;   )

;; (defn create-text-status!
;;   [xtdb-node user-id text reply-to async?]
;;   (let [{:keys [xt/id] :as new-status} (db/create-status user-id {:text text} {:reply-to reply-to})]
;;     (log/info "Creating text status" {:event :created-status :status-type :text :user-id user-id :content {:text text} :reply-to reply-to})
;;     (if async?
;;       (do
;;         (db-u/submit-tx xtdb-node [[:put-docs :mushin.db/statuses new-status]])
;;         (accepted {:message "Your post has been queued for creation"}))
;;       (do
;;         (db-u/execute-tx  xtdb-node [[:put-docs :mushin.db/statuses new-status]])
;;         (created (str "/statuses/" id) {:status-id id})))))

;; ;; TODO
;; (defn create-status-post!
;;   [{:keys [xtdb-node resource-map]}
;;    {{{{:keys [text media]} :content :keys [reply-to]} :body} :parameters {:keys [user-id]} :session :keys [mushin/async?] :as req}]
;;   (prn req)
;;   (when-not user-id
;;     (unauthorized! {:error :not-logged-in :message "You are not logged in, and so have no permissions to perform this action"}))
;;   (if media
;;     (create-media-status!)
;;     (create-text-status! xtdb-node user-id text reply-to async?)))
