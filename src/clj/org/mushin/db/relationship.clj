(ns org.mushin.db.relationship
  (:require [org.mushin.db.timestamps :as ts]
            [xtdb.api :as xt]
            [malli.core :as malli]
            [clj-uuid :as uuid]
            [java-time.api :as jt]
            [org.mushin.db.users :as users]
            [org.mushin.db.util :as db]))


(def ^:private relationship-types-schema
  "Different types of relationships between users.
  | Relationship | Meaning                                          |
  |:-------------|:-------------------------------------------------|
  | `:follow`    | A subscription to a user's feed                  |
  | `:block`     | A blocking of all communication between users    |
  | `:mute`      | For filtering out a user's posts from a timeline |
  "
  [:enum :follow :block :mute])

(def ^:private follow-states-schema
  "States for the following state machine:
  | Follow State      | Meaning                                      |
  |:------------------|:---------------------------------------------|
  | `:follow-pending` | A followee can accept a follow request.      |
  | `:follow-accept`  | The follower is following the followee.      |
  | `:follow-reject`  | The followee rejected the follower.          |
  | `:unfollow`       | The followee stopped following the follower. |

  This is used only by `:follow` relationships.
  "
  [:enum :follow-pending :follow-accept :follow-reject :unfollow])

;; Currently XTDB only supports UUIDs, ints, strings, and keywords as xt/ids,
;; but if they ever support tuples we should condense this schema into this:
;; [:xt/id [:tuple :keyword :uuid :uuid]], where keyword is the relationship type,
;; and the UUIDs are the IDs of the source and target users respectively.
;; This would also get rid of the requirement to ASSERT NOT EXIST when we
;; create a new relationship row.
(def ^:private base-realtionship-schema
  "Base schema for relationships.
  | Column        | Meaning                             |
  |:--------------|:------------------------------------|
  | `:xt/id`      | The ID of the row.                  |
  | `:source`     | The relationship source.            |
  | `:target`     | The relationship target.            |
  | `:type`       | The type of relationship.           |
  | `:created-at` | Time of relationship establishment. |
  "
  [:map
   [:xt/id                   :uuid]
   [:source                  :uuid]
   [:target                  :uuid]
   [:type                    relationship-types-schema]
   ts/created-at])

(def relationship-schema
  "Schema for the relationships table. Shares its base with `base-relationship-schema`.
  | Column   | Meaning                               |
  |:---------|:--------------------------------------|
  | `:state` | State in the following state machine. |
  "
  {:mushin.db/relationships
   [:multi {:dispatch (fn [{:keys [xt/id]}]
                        (first id))}
    [:block base-realtionship-schema]
    [:mute base-realtionship-schema]
    ;; Follow is the only one with a different schema.
    [:follow
     [:and
      base-realtionship-schema
      [:map
       [:state follow-states-schema]]]]]})

(defn relationship-doc
  [source target type & state]
  (cond-> {:xt/id (uuid/v7)
           :source source
           :target target
           :created-at (jt/zoned-date-time)
           :type type}
    state (assoc :state state)))

(defn insert-relation-tx
  "Create the transaction for inserting a relationship.

  # Arguments
   - `doc`: The relationship document to insert.

  # Return
  A transaction for inserting a relationship."
  [doc]
  (db/upsert-tx
   :mushin.db/relationships
   doc
   :type :source :target))

(defn delete-realtion-tx
  "Create a XTDB transaction that deletes a relationshp."
  [source target type]
  (db/delete-doc
   :mushin.db/relationships
   {:source source :target target :type type}
   :source :target :type))

(defn get-relationships
  "Get a collection of relationships with other users.

  # Arguments
   - `xtdb-node`: Database
   - `type`: The type of relationship to get
   - `source-id`: The source user

  # Optional arguments
   - `n`: How many rows to return (default 40)
   - `offset-by`: Offset into the ordering (default 0)
   - `order-column`: Which column to order by. Must be `:name` or `:created-at`
   - `reverse`: If true, reverses the ordering (default is descending)"
  [xtdb-node type source-id & {:keys [n offset-by order-column reverse]
                               :or {n 40
                                    offset-by 0}}]
  (xt/q xtdb-node [(xt/template
                    (fn [rel-source rel-type]
                      (-> (unify
                           (from :mushin.db/relationships [{:source rel-source :target rel-target :type rel-type :created-at relation-started}])
                           (from :mushin.db/users [nickname {:xt/id rel-target}]))
                          (order-by {:val ~(case order-column
                                             :name 'nickname
                                             :created-at 'relation-started)
                                     :dir ~(if reverse :asc :desc)
                                     :nulls :last})
                          (offset ~offset-by)
                          (limit ~n)
                          (return rel-target nickname relation-started))))
                   source-id type]))

(defn get-relationships-with
  [xtdb-node source-id target-id & {:keys [n offset-by order-column reverse]
                                    :or {n 40
                                         offset-by 0}}]
  (xt/q xtdb-node [(xt/template
                    (fn [rel-source rel-target]
                      (-> (unify
                           (from :mushin.db/relationships [{:source rel-source :target rel-target :created-at related-at}])
                           (from :mushin.db/users [nickname xt/id {:xt/id rel-target}]))
                          (order-by {:val ~(case order-column
                                             :name 'nickname
                                             :created-at 'related-at)
                                     :dir ~(if reverse :asc :desc)
                                     :nulls :last})
                          (offset ~offset-by)
                          (limit ~n))))
                   source-id target-id]))

(defn get-relationship-with
  [xtdb-node type source-id target-id]
  (first
   (xt/q xtdb-node [(xt/template
                     (fn [rel-type rel-source rel-target]
                       (-> (from :mushin.db/relationships [{:source rel-source :target rel-target :type rel-type}])
                           (limit 1))))
                    type source-id target-id])))

(defn has-relationship?
  "True if the source has a `type` relationhip with the target, otherwise false."
  [xtdb-node type source-id target-id]
  (boolean (get-relationship-with xtdb-node type source-id target-id)))


(defn can-follow-user?
  "Attempts to create a following relationship between a source user and a target user.

  Return value
   - `:can-request-follow`: The source can request to follow the target
   - `:can-follow`: The source can follow the target
   - `:following`: The source is following the target
   - `false`: The source is blocked by the target"
  [xtdb-node source-id target-id]
  (let [perm (users/check-user-can-view-user xtdb-node source-id target-id)]
    (if (or (= perm :allowed)
            (= perm :locked))
      (let [follows? (:state (get-relationship-with xtdb-node :follow source-id target-id))]
        (if (or (= :follow-pending follows?)
                (= :follow-accept follows?))
          follows?
          (if (= perm :allowed)
            :can-follow
            :can-request-follow)))
      false)))
