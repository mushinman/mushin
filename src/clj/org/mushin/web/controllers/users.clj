(ns org.mushin.web.controllers.users
  (:require [ring.util.http-response :refer [unauthorized! created ok not-found! accepted created]]
            [clojure.tools.logging :as log]
            [xtdb.api :as xt]
            [org.mushin.web.auth-utils :as auth-utils]
            [org.mushin.db.users :as db]))

(defn user-get-by-uuid
  [{:keys [xtdb-node]}
   {{{:keys [requested-user-id]} :body} :parameters {:keys [user-id]} :session}]
  (db/get-user-by-id xtdb-node requested-user-id))

(defn delete-user
  [{:keys [xtdb-node]}
   {{{:keys [id]} :path} :parameters {:keys [user-id]} :session}]
  (let [session-user user-id
        target-user id]
    (when-not target-user
      (not-found! {:error "post_not_found" :message "The post you were trying to delete was not found"}))
    (log/info {:event :delete-status :user user-id :status-owner target-user})
    (auth-utils/user-has-permissions-for! session-user target-user)
    (xt/submit-tx xtdb-node [[:delete-docs :mushin.db/statuses id]])
    (accepted {:message "The post has been queued for deletion"})))
