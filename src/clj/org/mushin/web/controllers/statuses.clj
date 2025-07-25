(ns org.mushin.web.controllers.statuses
  (:require [ring.util.http-response :refer [unauthorized! created ok not-found!]]
            [org.mushin.db.statuses :as db]
            [org.mushin.db.users :as user-db]
            [clojure.tools.logging :as log]
            [org.mushin.db.util :as db-u]
            [xtdb.api :as xt]
            [org.mushin.db.resources :as resources])
  (:import [java.nio.file Files]))

(def create-picture-post-body
  [:map {:closed true}
   [:image {:description "mulitpart file"}]
   [:text :string]])

(def create-text-post-body
  [:map {:closed true}
   [:text :string]])

(def get-timeline-query
  [:map {:closed true}
   [:reverse {:optional true}   :boolean]
   [:limit   {:optional true}   :int]
   [:offset  {:optional true}   :int]])


(defn get-timeline
  [{:keys [xtdb-node]}
   {{:keys [nickname]} :path-params {:keys [user-id]} :session :keys [query-params]}]
  ;; TODO check if the sessioned user is able to see this post.
  (let [user-id (or (user-db/get-user-id-by-nickname xtdb-node nickname)
                    (not-found! {:error "user_not_found" :message "A user by that nickname was not found"}))
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
   {{{:keys [id]} :path} :parameters}]
  (if-let [status (db/get-status-by-id xtdb-node id)]
    (ok {:status status})
    (not-found! {:error "status_not_found" :message (str "A status with the id " id " was not found")})))

(defn create-text-post!
  [{:keys [xtdb-node]}
   {{{:keys [text]} :body} :parameters {:keys [user-id]} :session :as req}]
  (when-not user-id
    (unauthorized! {:error "not_logged_in" :message "You are not logged in, and so have no permissions to perform this action"}))
  (let [{:keys [xt/id]} (db/create-status! xtdb-node {:text text} user-id)]
    (log/info "Created text status" {:event :created-status :status-type :text :user-id user-id :content {:text text}})
    (created (str "/statuses/" id) {:status-id id})))

(defn create-picture-post!
  [{:keys [xtdb-node]}
   {{{{:keys [image]} :status} :body} :parameters {:keys [user-id]} :session}]
  (when-not user-id
    (unauthorized! {:error "not_logged_in" :message "You are not logged in, and so have no permissions to perform this action"}))
  (let [{:keys [tempfile filename]} image
        {:keys [xt/id]} (resources/create-resource-from-file! xtdb-node tempfile filename)]
    (db/create-status! xtdb-node {:image id :text "Text"} user-id)))
