(ns org.mushin.web.controllers.i
  (:require [ring.util.http-response :refer [not-found! accepted no-content unauthorized! conflict! ok]]
            [org.mushin.db.authorization :as db-authz]
            [malli.experimental.time :as mallt]
            [clojure.tools.logging :as log]
            [org.mushin.web.auth-utils :as auth]
            [org.mushin.db.likes :as likes]
            [xtdb.api :as xt]
            [org.mushin.db.util :as db]
            [org.mushin.db.timeline :as db-timeline]
            [org.mushin.db.users :as db-users]
            [org.mushin.db.relationship :as rel]
            [org.mushin.db.users :as users]))

(def any-timestamp
  "A schema for any date object that xtdb supports."
  [:or
   (mallt/-zoned-date-time-schema)
   (mallt/-instant-schema)]) ; TODO Maybe we can support more types with a value transformer...


(defn collection-query-schema
  "Create a schema for api calls that return collections."
  [& {:keys [n-max]
      :or {n-max 20}}]
  [:map
   [:reverse      {:optional true} :boolean]
   [:order-column {:optional true} [:enum :created-at :name]]
   [:n            {:optional true} [:int {:min 0 :max n-max}]]])

(def bitemporal-query-schema
  "Schema for bitemporal queries."
  [:map
   [:order-column {:optional true} [:enum :valid-from :valid-to :system-from :system-to]]
   [:valid {:optional true} [:or
                             [:map [:at any-timestamp]]
                             [:map [:in [:tuple any-timestamp any-timestamp]]]
                             [:map [:from any-timestamp]]
                             [:map [:to any-timestamp]]]]
   [:system {:optional true} [:or
                              [:map [:at any-timestamp]]
                              [:map [:in [:tuple any-timestamp any-timestamp]]]
                              [:map [:from any-timestamp]]
                              [:map [:to any-timestamp]]]]])

(def bitemporal-collection-query-schema
  "Schema for bitemporal queries on collections."
  [:or
   bitemporal-query-schema
   (collection-query-schema)])


(def delete-me-body-schema
  [:map
   [:password :string]])

(defn get-user
  [{:keys [xtdb-node]}
   {{:keys [user-id]} :session}]
  (if-let [user-doc
           (db-users/get-user-by-id
            xtdb-node
            user-id
            '[xt/id email log-counter nickname bio privacy-level local? joined-at last-logged-in-at state])]
    (ok user-doc)
    (not-found! {:error :user-not-found :message "The user you tried to find was not found."})))

(defn get-roles
  [{:keys [xtdb-node]}
   {{:keys [user-id]} :session}]
  (db-authz/actor-roles xtdb-node user-id))

(defn delete-self!
  [{:keys [xtdb-node]}
   {{{:keys [password]} :body} :parameters {:keys [user-id]} :session
    :keys [mushin/async?]}]
  ;; TODO also invalidate the session state for the deleted user.
  (log/info {:event :delete-me :user-id user-id})
  (let [nick (:nickname (db-users/get-user-by-id xtdb-node user-id))]
    (log/info {:delete-me-part2 "yup" :nick nick})
    (when-not (users/check-nickname-and-password xtdb-node nick password)
      (unauthorized! {:error :invalid-password}))
    (if async?
      (do
        (db/submit-tx xtdb-node (db-users/delete-user-tx user-id))
        (accepted))
      (do
        (db/execute-tx xtdb-node (db-users/delete-user-tx user-id))
        (no-content)))))

(defn create-status
  ;; TODO
  [{:keys [xtdb-node]}
   {{{:keys []} :body} :parameters
    {:keys [user-id]} :session
    :keys [mushin/async?]}]
  )

(defn get-timeline
  [{:keys [xtdb-node]}
   {{:keys [user-id]} :session}]
  (xt/q xtdb-node (db-timeline/get-user-events user-id)))

(defn get-likes
  [{:keys [xtdb-node]}
   {{{:keys [valid order-column offset-by n reverse]
      :or {order-column 'created-at
           offset-by    0
           n            20}} :query} :parameters
    {:keys [user-id]}  :session}]
  (xt/q
   xtdb-node
   [(xt/template
     (fn [actor-id]
       (->
        (from :mushin.db/likes
              ~(db/query-bind '[xt/id status-id {:actor-id actor-id
                                                 :xt/valid-from valid-from
                                                 :xt/valid-to vto}]
                              valid nil))
        (order-by {:val ~(case order-column
                           :valid-from 'valid-from
                           :valid-to 'valid-to
                           'created-at)
                   :dir ~(if reverse :asc :desc)
                   :nulls :last})
        (offset ~offset-by)
        (limit ~n)
        (without ~(if valid :__nothing_ever_happens :valid-from)
                 ~(if valid :__nothing_ever_happens :valid-to)))))
    user-id]))

(defn block-user!
  "API handler for blocking a user."
  [{:keys [xtdb-node]}
   {{{:keys [id]} :path} :parameters
    {:keys [user-id]}  :session
    :keys [mushin/async?]}]
  (when (rel/has-relationship? xtdb-node :block user-id id)
    (conflict! {:error :user-already-blocked :message "You have already blocked that user" :user-id id}))
  (if async?
    (db/submit-tx xtdb-node (rel/insert-relation-tx (rel/relationship-doc :block user-id id)))
    (db/execute-tx xtdb-node (rel/insert-relation-tx (rel/relationship-doc :block user-id id))))
  (no-content))


(defn mute-user!
  "API handler for muting a user."
  [{:keys [xtdb-node]}
   {{{:keys [id]} :path} :parameters
    {:keys [user-id]}  :session
    :keys [mushin/async?]}]
  (when (rel/has-relationship? xtdb-node :mute user-id id)
    (conflict! {:error :user-already-muted :message "You have already muted that user" :user-id id}))
  (if async?
    (db/submit-tx xtdb-node (rel/insert-relation-tx (rel/relationship-doc :mute user-id id)))
    (db/execute-tx xtdb-node (rel/insert-relation-tx (rel/relationship-doc :mute user-id id))))
  (no-content))

(defn follow-user!
  "API handler for following a user."
  [{:keys [xtdb-node]}
   {{{:keys [id]} :path} :parameters
    {:keys [user-id]}  :session
    :keys [mushin/async?]}]
  (if-let [can-follow (rel/can-follow-user? xtdb-node user-id id)]
    (case
        :following
      (conflict! {:error :user-already-muted :message "You have already muted that user" :user-id id})

      (:can-follow :can-request-follow)
      (if async?
        (db/submit-tx xtdb-node (rel/insert-relation-tx (rel/relationship-doc :follow user-id id can-follow)))
        (db/execute-tx xtdb-node (rel/insert-relation-tx (rel/relationship-doc :follow user-id id can-follow))))
      (no-content))
    (unauthorized! (auth/user-blocked id))))

(defn unmute-user!
  [{:keys [xtdb-node]}
   {{{:keys [id]} :path} :parameters
    {:keys [user-id]}  :session
    :keys [mushin/async?]}]
  (if async?
    (xt/submit-tx xtdb-node [(rel/delete-realtion-tx :mute user-id id)])
    (xt/execute-tx xtdb-node [(rel/delete-realtion-tx :mute user-id id)]))
  (no-content))

(defn unblock-user!
  [{:keys [xtdb-node]}
   {{{:keys [id]} :path} :parameters
    {:keys [user-id]}  :session
    :keys [mushin/async?]}]
  (if async?
    (xt/submit-tx xtdb-node [(rel/delete-realtion-tx :block user-id id)])
    (xt/execute-tx xtdb-node [(rel/delete-realtion-tx :block user-id id)]))
  (no-content))


(defn unfollow-user!
  [{:keys [xtdb-node]}
   {{{:keys [id]} :path} :parameters
    {:keys [user-id]}  :session
    :keys [mushin/async?]}]
  (if async?
    (xt/submit-tx xtdb-node (rel/insert-relation-tx (rel/relationship-doc user-id id :follow :unfollow)))
    (xt/execute-tx xtdb-node (rel/insert-relation-tx (rel/relationship-doc user-id id :follow :unfollow))))
  (no-content))

(defn get-blocked-accounts
  [{:keys [xtdb-node]}
   {{{:keys [order-column offset-by n reverse]
      :or {order-column :created-at
           offset-by    0
           n            20}} :query} :parameters
    {:keys [user-id]} :session}]
  (rel/get-relationships xtdb-node :block user-id :n n :offset-by offset-by :order-column order-column :reverse reverse))


(defn get-muted-accounts
  [{:keys [xtdb-node]}
   {{{:keys [order-column offset-by n reverse]
      :or {order-column :created-at
           offset-by    0
           n            20}} :query} :parameters
    {:keys [user-id]} :session}]
  (rel/get-relationships xtdb-node :muted user-id :n n :offset-by offset-by :order-column order-column :reverse reverse))
