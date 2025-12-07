(ns org.mushin.db.statuses
  (:require [org.mushin.db.util :as db]
            [java-time.api :as jt]
            [honey.sql.helpers :as h]
            [honey.sql :as sql]
            [org.mushin.db.authorization :as authz]
            [clj-uuid :as uuid]
            [xtdb.api :as xt]
            [org.mushin.utils :refer [to-java-uri]]
            [lambdaisland.uri :refer [join]]
            [org.mushin.db.timestamps :as timestamps]
            [org.mushin.db.util :as db-util]))

(def status-types-schema
  [:enum :text :image :animated-image :video :comic :microblog :tombstone])


(def statuses-schema
  "Schema for statuses.
  | Key          | Type                              | Meaning                               |
  |:-------------|:----------------------------------|:--------------------------------------|
  | `xt/id`      | UUID                              | Row Key                               |
  | `creator`    | UUID/Foreign key to `users` table | Owner of the status                   |
  | `reply-to`   | UUID/Key for `statuses` table     | Status that this status is a reply to |
  | `created-at` | Timestamp                         | The time a user created this post     |
  | `updated-at` | Timestamp                         | The time a user edited this post      |
  | `content`    | Text and/or media                 | The content of the post               |
  "
  {:mushin.db/statuses
   [:map
    [:type                      status-types-schema]
    [:xt/id                     :uuid]
    [:creator                   :uuid]
    [:reply-to {:optional true} :uuid]
    [:ap-id                     uri?]
    timestamps/created-at
    timestamps/updated-at
    [:content                   [:multi {:dispatch :type}
                                 [:hiccup ;; Hiccup DSL.
                                  [:map
                                   [:type     :keyword]
                                   [:content  [:every :any]]]]
                                 [:html ;; HTML as a string.
                                  [:map
                                   [:type    :keyword]
                                   [:content :string]]]]]
    ;authz/authorization-object-schema
    ]})

(defn get-statuses-by-user
  [xtdb-node user-id]
  (xt/q xtdb-node (xt/template
                   (from :mushin.db/statuses [* {:creator ~user-id}]))))

(defn inter-users-statuses-tx
  "Create a XTDB transaction part that replaces every status by a particular user with a tombstone."
  [user-id]
  (let [[q & params]
        (-> {:update :mushin.db/statuses
             :set {:type [:raw "?"]
                   :updated-at :current-timestamp}
             :where [:= :creator user-id]}
            sql/format)]
    [:sql q (vec (concat [:tombstone] params))]))

(defn create-status
  [user-id content type ap-id-prefix & opt-status]
  (let [{:keys [reply-to created-at updated-at]} (first opt-status)
        now (jt/zoned-date-time)
        id (random-uuid)]
    (cond-> (merge {:xt/id      id
                    :type       type
                    :ap-id      (to-java-uri (join ap-id-prefix id))
                    :creator    user-id
                    :created-at (or created-at now)
                    :updated-at (or updated-at now)
                    :content    content})
      reply-to (assoc :reply-to reply-to))))

(defn insert-status-tx
  [doc]
  [[:put-docs :mushin.db/statuses doc]])

(defn get-status-by-id
  ([xtdb-node cols id] (db/lookup-by-id xtdb-node :mushin.db/statuses cols id))
  ([xtdb-node id] (get-status-by-id xtdb-node '[*] id)))

(defn get-comments-for-status
  ([xtdb-node cols id] (xt/q xtdb-node (xt/template (from :mushin.db/statuses [~@cols {:reply-to ~id}]))))
  ([xtdb-node id] (get-comments-for-status xtdb-node '[*] id)))

(defn get-n-statuses-by-user
  ([xtdb-node user-id n offset] (get-n-statuses-by-user xtdb-node user-id n offset :desc))
  ([xtdb-node user-id n offset direction]
   (xt/q xtdb-node (xt/template (-> (from :mushin.db/statuses [* {:creator ~user-id}])
                                    (order-by {:val created-at, :dir ~direction})
                                    (offset ~offset)
                                    (limit ~n))))))

(defn get-n-status-ids-by-user
  ([xtdb-node user-id n offset] (get-n-status-ids-by-user xtdb-node user-id n offset :desc))
  ([xtdb-node user-id n offset direction]
   (xt/q xtdb-node (xt/template (-> (from :mushin.db/statuses [xt/id created-at {:creator ~user-id}])
                                    (order-by {:val created-at, :dir ~direction})
                                    (without created-at)
                                    (offset ~offset)
                                    (limit ~n))))))
