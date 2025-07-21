(ns org.mushin.web.controllers.statuses
  (:require [ring.util.http-response :refer [unauthorized!]]
            [org.mushin.schema.statuses :as schema]
            [org.mushin.db.statuses :as db]
            [ring.util.http-response :refer [created]]
            [clojure.tools.logging :as log]
            [org.mushin.db.resources :as resources])
  (:import [java.nio.file Files]))

(def create-picture-post-body
  [:map {:closed true}
   [:image {:description "mulitpart file"}]
   [:text :string]])

(def create-text-post-body
  [:map {:closed true}
   [:text :string]])

(defn create-text-post!
  [{:keys [xtdb-node]}
   {{{:keys [text]} :body} :parameters :keys [user-id]}]
  (when-not user-id
    (unauthorized! {:error "not_logged_in" :message "You are not logged in, and so have no permissions to perform this action"}))
  (let [{:keys [xt/id]} (db/create-status! xtdb-node {:text text} user-id)]
    (created (str "/statuses/" id) {:status-id id})))

(defn create-picture-post!
  [{:keys [xtdb-node]}
   {{{{:keys [image]} :status} :body} :parameters :keys [user-id]}]
  (when-not user-id
    (unauthorized! {:error "not_logged_in" :message "You are not logged in, and so have no permissions to perform this action"}))
  (let [{:keys [tempfile filename]} image
        {:keys [xt/id]} (resources/create-resource-from-file! xtdb-node tempfile filename)]
    (db/create-status! xtdb-node {:image id :text "Text"} user-id)))
