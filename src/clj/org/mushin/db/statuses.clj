(ns org.mushin.db.statuses
  (:require [org.mushin.db.util :as db]
            [java-time.api :as jt]
            [honey.sql.helpers :as h]
            [honey.sql :as sql]
            [org.mushin.db.authorization :as authz]
            [clj-uuid :as uuid]
            [org.mushin.db.users :as db-users]
            [xtdb.api :as xt]
            [org.mushin.utils :refer [to-java-uri]]
            [lambdaisland.uri :refer [join]]
            [org.mushin.db.timestamps :as timestamps]
            [org.mushin.db.util :as db-util]))

(def status-types-schema
  [:enum :text :image :animated-image :video :comic :microblog :tombstone])

(def status-encodings-schema
  [:enum :hiccup :svg :html :resource])

(def statuses-schema
  "Schema for statuses.
  | Key                | Type                              | Meaning                                                            |
  |:-------------------|:----------------------------------|:-------------------------------------------------------------------|
  | `xt/id`            | UUID                              | Row Key                                                            |
  | `primary-encoding` | keyword                           | The default post content encdoing, e.g. `:html`, `:hiccup`         |
  | `creator`          | UUID/Foreign key to `users` table | Owner of the status                                                |
  | `reply-to`         | UUID/Key for `statuses` table     | Status that this status is a reply to                              |
  | `created-at`       | Timestamp                         | The time a user created this post                                  |
  | `updated-at`       | Timestamp                         | The time a user edited this post                                   |
  | `content`          | Map                               | The content of the post                                            |
  | `ap-id`            | URI                               | ActivityPub ID/location of the status resource                     |
  | `mentions`         | Set of UUID                       | Set of user IDs mentioned in the status                            |
  | `resources`        | Set of UUID                       | Set of resource metadata IDs for each resources used in the status |
  `content` is a map where each key is a version of the post in some format, e.g. `:hiccup` for
  hiccup syntax, or `:html` for raw html, or `:svg`, etc..
  "
  {:mushin.db/statuses
   [:map
    [:type                      status-types-schema]
    [:primary-encoding          status-encodings-schema]
    [:xt/id                     :uuid]
    [:creator                   :uuid]
    [:reply-to {:optional true} :uuid]
    [:ap-id                     uri?]
    [:mentions                  [:set :uuid]]
    timestamps/created-at
    timestamps/updated-at
    [:content                   :map]
    [:resources                 [:set :uuid]]
    ;authz/authorization-object-schema
    ]})


(def ^:private select-columns
  '[xt/id created-at updated-at creator type ap-id primary-encoding
    mentions content])

(defn get-statuses-by-user
  [xtdb-node user-id]
  (xt/q xtdb-node (xt/template
                   (from :mushin.db/statuses [* {:creator ~user-id}]))))

(defn delete-users-statuses-tx
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
  [user-id content type primary-encoding ap-id-prefix resources &
   {:keys [reply-to created-at updated-at
           mentions]}]
  (let [now (jt/zoned-date-time)
        id (random-uuid)]
    (cond-> (merge {:xt/id      id
                    :type       type
                    :ap-id      (to-java-uri (join ap-id-prefix id))
                    :primary-encoding primary-encoding
                    :mentions   (or mentions #{})
                    :creator    user-id
                    :created-at (or created-at now)
                    :updated-at (or updated-at now)
                    :content    content})
      reply-to (assoc :reply-to reply-to))))

(defn insert-status-tx
  [doc]
  [[:put-docs :mushin.db/statuses doc]])

(defn get-status-primary-content
  ([db-con id encoding]
   (first 
    (xt/q db-con
          [(xt/template
            (fn [id encoding]
              (-> (from :mushin.db/statuses [~@select-columns {:xt/id id}])
                  (with {:primary-content
                         (case encoding
                           :html (. content html)
                           :svg (. content svg)
                           :resource (. content resource)
                           :hiccup (. content hiccup))})
                  (without :content))))
           id encoding])))
  ([db-con id]
   (first 
    (xt/q db-con
          [(xt/template
            (fn [id]
              (-> (from :mushin.db/statuses [~@select-columns primary-encoding content {:xt/id id}])
                  (with {:primary-content
                         (case primary-encoding
                           :resource (. content resource)
                           :html (. content html)
                           :svg (. content svg)
                           :hiccup (. content hiccup))})
                  (without :content))))
           id]))))

(defn get-status-primary-content-for-user
  "Get a status by its ID, checking if the viewer has permissions to access the status.

  # Arguments
  - `db-con`: Database connection.
  - `status-id`: ID of the status to fetch for the viewer.
  - `viewer-id?`: ID of the viewer. Can be `nil`.

  # Return value
  A map of the following structure:
| Key        | Type    | Note                                                                             |
|:-----------|:--------|:---------------------------------------------------------------------------------|
| Permission | Keyword | `:allowed` if the viewer can view the status. `:blocked` or `:locked` otherwise. |
| Status     | Map     | Optional. The status. Only present if `:permission` is `:allowed`.               |

  Or `nil` if the status does not exist.
  "
  [db-con status-id viewer-id? encoding?]
  (when-let [{:keys [creator] :as status}
             (if encoding?
               (get-status-primary-content db-con status-id encoding?)
               (get-status-primary-content db-con status-id))]
    (let [permission (db-users/check-user-can-view-user db-con viewer-id? creator)]
      (cond-> {:permission permission}
        (= permission :allowed) (assoc :status status)))))


(defn get-statuses-by-creator-primary-content
  ([db-con creator-id & {:keys [n cursor reverse prev-page order-column]
                         :or {n 10
                              order-column :created-at}}]
   (let [;; Asc by default, and reverse and prev-page cancel each other out.
         order-direction (if reverse :desc :asc)

         cursor-dir (if prev-page (if reverse :asc :desc) (if reverse :desc :asc))

         {[cursor-column-name cursor-value] :last :keys [cursor-id]} cursor

         cursor-column-name (or cursor-column-name order-column :created-at) ; Default value.

         cursor-column (case cursor-column-name
                         :created-at 'created-at
                         :updated-at 'updated-at)

         result
         (xt/q
          db-con
          [(xt/template
            (fn [cursor-value cursor-id n
                 backwards order-direction no-cursor
                 creator-id]
              (->
               (from :mushin.db/statuses [~@select-columns
                                          {:creator creator-id}])
               (where
                (or no-cursor
                    (if (or (and (not backwards) (= order-direction :asc))
                            (and backwards (= order-direction :desc)))
                      (or (> ~cursor-column cursor-value)
                          (and (= ~cursor-column cursor-value) (> xt/id cursor-id)))
                      (or (< ~cursor-column cursor-value)
                          (and (= ~cursor-column cursor-value) (< xt/id cursor-id))))))
               (order-by {:val ~cursor-column
                          :dir ~cursor-dir
                          :nulls :last}
                         {:val xt/id
                          :dir ~cursor-dir}) 
               (limit n)
               (with {:primary-content
                      (case primary-encoding
                        :resource (. content resource)
                        :html (. content html)
                        :svg (. content svg)
                        :hiccup (. content hiccup))})
               (without :content))))
           cursor-value cursor-id n (boolean prev-page)
           order-direction (not (boolean cursor)) creator-id])

         {:keys [xt/id created-at updated-at] :as last-row} (last result)]
     (cond-> {:result result}
       (or last-row (< n (count result)))
       (assoc :cursor {:cursor-id id
                       :last [cursor-column-name (case cursor-column-name
                                                   :created-at created-at
                                                   :updated-at updated-at)]})))))

(defn delete-all-posts-by-tx
  [creator-id]
  (db-util/delete-where :mushin.db/statuses
                        (xt/template
                         (->
                          (from :mushin.db/statuses [{:creator ~creator-id}])
                          (limit 1)))))

(defn get-status-by-id
  ([xtdb-node cols id] (db/lookup-by-id xtdb-node :mushin.db/statuses cols id))
  ([xtdb-node id] (get-status-by-id xtdb-node '[*] id)))


(defn living-status?
  "Check if status exists and is not a tombstone."
  [db-con id]
  (let [{:keys [alive?] :as result}
        (xt/q
         db-con
         [(xt/template
           (fn [id]
             (-> (from :mushin.db/statuses [{:xt/id id :type type}])
                 (limit 1)
                 (return {:alive? (<> type :tombstone)}))))
          id])]
    (boolean (and (not-empty result) alive?))))

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
