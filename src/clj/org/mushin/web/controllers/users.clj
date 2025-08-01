(ns org.mushin.web.controllers.users
  (:require [ring.util.http-response :refer [unauthorized! created ok not-found! accepted created no-content]]
            [clojure.tools.logging :as log]
            [xtdb.api :as xt]
            [org.mushin.web.auth-utils :as auth-utils]
            [org.mushin.db.users :as db]))

(defn user-get-by-uuid
  [{:keys [xtdb-node]}
   {{{:keys [requested-user-id]} :body} :parameters {:keys [user-id]} :session}]
  (db/get-user-by-id xtdb-node requested-user-id))

(defn delete-user!
  [{:keys [xtdb-node]}
   {{{:keys [id]} :path} :parameters {:keys [user-id]} :session :keys [mushin/async?] :as req}]
  (let [session-user user-id
        target-user id]
    (when-not target-user
      (not-found! {:error :user-not-found :message "The user you were trying to delete was not found"}))
    (log/info {:event :delete-user :user user-id})
    (auth-utils/user-has-permissions-for! session-user target-user)

    ;; TODO maybe queue the user's posts for deletion too?
    (if async?
      (do
        (xt/submit-tx xtdb-node [[:delete-docs :mushin.db/statuses id]])
        (accepted {:message "This user has been queued for deletion"}))
      (do
        (xt/execute-tx xtdb-node [[:delete-docs :mushin.db/statuses id]])
        (no-content {:message "This user has been deleted"})))))
