(ns org.mushin.web.controllers.i
  (:require [ring.util.http-response :refer [not-found! accepted no-content unauthorized!]]
            [org.mushin.db.authorization :as db-authz]
            [org.mushin.passwords :as passw]
            [org.mushin.db.util :as db]
            [org.mushin.db.users :as db-users]))

(def inter-body-schema
  [:map
   [:password :string]])

(defn get-user
  [{:keys [xtdb-node]}
   {{:keys [user-id]} :session}]
  (if-let [user-doc (db-users/get-user-by-id xtdb-node user-id)]
    (select-keys user-doc [:xt/id :email :log-counter :nickname :bio :privacy-level :local? :joined-at :last-logged-in-at])
    (not-found! {:error :user-not-found :message "The user you tried to find was not found."})))

(defn get-roles
  [{:keys [xtdb-node]}
   {{:keys [user-id]} :session}]
  (db-authz/actor-roles xtdb-node user-id))

; TODO move password from request body to basic auth.
(defn delete-self!
  [{:keys [xtdb-node]}
   {{{:keys [password]} :body} :parameters {:keys [user-id]} :session :keys [mushin/async?]}]
  (when-not (passw/nickname-and-password-are-valid? xtdb-node (:nickname (db-users/get-user-by-id xtdb-node user-id)) password)
    (unauthorized! {:error :invalid-password}))
  (if async?
    (do
      (db/submit-tx xtdb-node (db-users/delete-user-tx user-id))
      (accepted))
    (do
      (db/execute-tx xtdb-node (db-users/delete-user-tx user-id))
      (no-content))))

(defn create-status
  [{:keys [xtdb-node]}
   {{{:keys [password]} :body} :parameters {:keys [user-id]} :session :keys [mushin/async?]}]
  )
