(ns org.mushin.web.controllers.i
  (:require [ring.util.http-response :refer [bad-request! not-found! accepted
                                             no-content unauthorized! conflict! ok]]
            [org.mushin.db.authorization :as db-authz]
            [malli.experimental.time :as mallt]
            [org.mushin.db.media :as media]
            [org.mushin.files :as files]
            [clojure.tools.logging :as log]
            [org.mushin.db.likes :as likes]
            [xtdb.api :as xt]
            [org.mushin.db.util :as db]
            [org.mushin.db.timeline :as db-timeline]
            [org.mushin.db.relationship :as rel]
            [org.mushin.db.users :as users]
            [java-time.api :as time]))

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
   [:cursor {:optional true}
    [:multi {:dispatch (fn [{:keys [last]}] (first last))}
     [:at
      [:map
       [:cursor-id :uuid]
       [:last [:tuple :keyword any-timestamp]]]]
     [:nickname
      [:map
       [:cursor-id :uuid]
       [:last [:tuple :keyword :string]]]]]]
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

(def statuses-body-schema
  "Schema for creating statuses."
  [:map
   [:multi {:dispatch :type}
    [:microblog
     [:map
      [:media {:description "mulitpart file" :optional true} :any]
      [:text  {:optional true} :string]]]
    [:meme
     [:map
      [:media {:description "mulitpart file"} :any]
      [:text :string]]]]])


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
           (users/get-user-by-id
            xtdb-node
            user-id
            '[xt/id email log-counter nickname bio privacy-level local? joined-at last-logged-in-at state])]
    (ok user-doc)
    (not-found! {:error :user-not-found :message "The user you tried to find was not found."})))

(defn get-roles ; TODO
  [{:keys [xtdb-node]}
   {{:keys [user-id]} :session}]
  (db-authz/actor-roles xtdb-node user-id))

(defn delete-self!
  [{:keys [xtdb-node]}
   {{{:keys [password]} :body} :parameters {:keys [user-id]} :session
    :keys [mushin/async?]}]
  ;; TODO also invalidate the session state for the deleted user.
  (log/info {:event :delete-me :user-id user-id})
  (let [nick (:nickname (users/get-user-by-id xtdb-node user-id))]
    (log/info {:delete-me-part2 "yup" :nick nick})
    (when-not (users/check-nickname-and-password xtdb-node nick password)
      (unauthorized! {:error :invalid-password}))
    (if async?
      (do
        (db/submit-tx xtdb-node (users/delete-user-tx user-id))
        (accepted))
      (do
        (db/execute-tx xtdb-node (users/delete-user-tx user-id))
        (no-content)))))

(defn create-microblog!
  [xtdb-node async? {:keys [media text]}]
  )

(defn create-meme!
  [xtdb-node async? resource-map {{:keys [tempfile]} :media
                                  :keys [text]}]
  (if-not (files/is-child-of tempfile files/tmp-dir)
    (bad-request! {:error :invalid-upload
                   :message "Something about the upload is wrong"})
    (try
      (let [content-type (files/detect-content-type tempfile)]
        (case content-type
          ("image/jpeg" "image/png")
          (media/create-captioned-resource-from-static-image! tempfile resource-map content-type text)
            (bad-request! {:error :invalid-content-type
                           :message "The content type isn't supported"
                           :content-type content-type})))
      (finally
        (files/delete-if-exists tempfile)))))

(defn create-status!
  [{:keys [xtdb-node resource-map]}
   {{{:keys [type] :as body} :body} :parameters
    {:keys [user-id]} :session
    :keys [mushin/async?]}]
  (case type
    :microblog (create-microblog! xtdb-node async? body)
    :meme (create-meme! xtdb-node resource-map async? body)))

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
    (db/compose-and-submit-txs! xtdb-node (rel/insert-block-tx user-id id))
    (db/compose-and-execute-txs! xtdb-node (rel/insert-block-tx user-id id)))
  (no-content))

(defn mute-user!
  "API handler for blocking a user."
  [{:keys [xtdb-node]}
   {{{:keys [id]} :path} :parameters
    {:keys [user-id]}  :session
    :keys [mushin/async?]}]
  (when (rel/has-relationship? xtdb-node :block user-id id)
    (conflict! {:error :user-already-muted :message "You have already muted that user" :user-id id}))
  (if async?
    (db/compose-and-submit-txs! xtdb-node (rel/insert-mute-tx user-id id))
    (db/compose-and-execute-txs! xtdb-node (rel/insert-mute-tx user-id id)))
  (no-content))


(defn follow-user!
  "API handler for following a user."
  [{:keys [xtdb-node]}
   {{{:keys [id]} :path} :parameters
    {:keys [user-id]}  :session
    :keys [mushin/async?]}]
  (let [can-follow (rel/follow-tx xtdb-node user-id id)]
    (if (vector? can-follow)
      (do
        (if async?
          (db/compose-and-submit-txs! xtdb-node can-follow)
          (db/compose-and-execute-txs! xtdb-node can-follow))
        (no-content))

      (case can-follow
        :following
        (conflict! {:error :user-already-muted
                    :message "You have already muted that user"
                    :user-id user-id
                    :target-id id})

        :self-follow
        (bad-request! {:error :self-follow
                       :message "You cannot follow yourself"
                       :user-id user-id
                       :target-id id})

        :denied
        (unauthorized! {:error :denied
                        :message "You cannot follow that user"
                        :user-id user-id
                        :target-id id})))))

(defn follows?
  [{:keys [xtdb-node]}
   {{{:keys [id]} :path} :parameters
    {:keys [user-id]}    :session}]
  )

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
  (let [unfollow-tx (rel/unfollow-tx xtdb-node user-id id)]
    (if (vector? unfollow-tx)
      (do
        (if async?
          (db/compose-and-submit-txs! xtdb-node unfollow-tx)
          (db/compose-and-execute-txs! xtdb-node unfollow-tx))
        (no-content))
      (bad-request!
       {:error unfollow-tx
        :message (case unfollow-tx
                   :not-following
                   "You cannot unfollow a user you are not following"

                   :self-unfollow
                    "You cannot unfollow yourself")}))))


(defn get-following
  [{:keys [xtdb-node]}
   {{{:keys [order-column offset-by n reverse cursor]
      :or {order-column :created-at
           offset-by    0
           n            20}
      :as q} :query} :parameters
    {:keys [user-id]} :session}]

  (ok (rel/get-related-accounts xtdb-node :follow user-id true q)))

(defn get-followers
  [{:keys [xtdb-node]}
   {{{:keys [order-column offset-by n reverse]
      :or {order-column :created-at
           offset-by    0
           n            20}
      :as q} :query} :parameters
    {:keys [user-id]} :session}]
  (ok (rel/get-related-accounts xtdb-node :follow user-id false q)))

(defn get-blocked-accounts
  [{:keys [xtdb-node]}
   {{{:keys [order-column offset-by n reverse]
      :or {order-column :created-at
           offset-by    0
           n            20}
      :as q} :query} :parameters
    {:keys [user-id]} :session}]
  (ok (rel/get-related-accounts xtdb-node :block user-id true q)))


(defn get-muted-accounts
  [{:keys [xtdb-node]}
   {{{:keys [order-column offset-by n reverse]
      :or {order-column :created-at
           offset-by    0
           n            20}
      :as q} :query} :parameters
    {:keys [user-id]} :session}]
  (ok (rel/get-related-accounts xtdb-node :mute user-id true q)))
