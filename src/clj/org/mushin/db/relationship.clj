(ns org.mushin.db.relationship
  (:require [org.mushin.db.timestamps :as ts]
            [xtdb.api :as xt]
            [malli.core :as malli]
            [clj-uuid :as uuid]
            [java-time.api :as jt]
            [org.mushin.db.users :as users]
            [org.mushin.db.util :as db])
  (:import [java.time Instant]))


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
   [:multi {:dispatch :type}
    [:block base-realtionship-schema]
    [:mute base-realtionship-schema]
    ;; Follow is the only one with a different schema.
    [:follow
     [:and
      base-realtionship-schema
      [:map
       [:state follow-states-schema]]]]]})

(defn relationship-doc
  ([source target type state]
   (cond-> {:xt/id (uuid/v7)
            :source source
            :target target
            :created-at (jt/zoned-date-time)
            :type type}
     state (assoc :state state)))
  ([source target type]
   (relationship-doc source target type nil)))

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
   [:type :source :target]))

(def ^:private delete-where-cols
  "Column names that are used for upsert."
  [:type :source :target])

(defn delete-mute-tx
  [source target]
  (db/delete-where
   :mushin.db/relationships
   (xt/template (-> (from :mushin.db/relationships [{:type :follow
                                                     :source ~source
                                                     :target ~target}])
                    (limit 1)))))

(defn insert-mute-tx
  [source target]
  (db/upsert-tx :mushin.db/relationships
                (relationship-doc source target :mute)
                delete-where-cols))

(defn delete-block-tx
  [source target]
  (db/compose-txs
   (db/delete-where
    :mushin.db/relationships
    (xt/template (-> (from :mushin.db/relationships [{:type :block
                                                      :source ~source
                                                      :target ~target}])
                     (limit 1))))))

(defn insert-block-tx
  "Create a tx that blocks `target` from `source`.

  This tx will also mutually unfollow `source` and `target`."
  [source target]
  (db/compose-txs
   (db/delete-where
    :mushin.db/relationships
    (xt/template (-> (from :mushin.db/relationships [{:type :follow
                                                      :source ~target
                                                      :target ~source}])
                     (limit 1))))
   (db/delete-where
    :mushin.db/relationships
    (xt/template (-> (from :mushin.db/relationships [{:type :follow
                                                      :source ~source
                                                      :target ~target}])
                     (limit 1))))

   (db/upsert-tx :mushin.db/relationships
                 (relationship-doc source target :block)
                 delete-where-cols)))

(defn insert-unfollow-tx
  [source target]
  (db/compose-txs
   ;; Delete the following row and insert follow.
   (db/delete-where
    :mushin.db/relationships
    (xt/template (-> (from :mushin.db/relationships [{:type :follow
                                                      :source ~source
                                                      :target ~target}])
                     (limit 1))))

   (db/upsert-tx :mushin.db/relationships
                 (relationship-doc source target :unfollow)
                 delete-where-cols)))

(defn insert-follow-tx
  [source target pending?]
  (db/compose-txs
   ;; Ensure the target hasn't blocked the user.
   (db/assert-not-exists-tx :mushin.db/relationships {:source target
                                                      :target source
                                                      :type :block})
   ;; Delete any previous following relationships and insert.
   (db/upsert-tx :mushin.db/relationships
                 (relationship-doc source target :follow (if pending? :follow-pending :follow-accept))
                 delete-where-cols)))

(defn delete-realtion-tx
  "Create a XTDB transaction that deletes a relationshp."
  [type source target]
  (db/delete-doc
   :mushin.db/relationships
   {:source source :target target :type type}
   [:source :target :type]))

(defn- stateful?
  "True if the relationship has a state column, false if not."
  [rel]
  (case rel
    :follow true
    false))

(defn- get-column
  [name]
  (case name
    :at 'at
    :nickname 'nickname))

(defn get-related-accounts
  "Get a collection of accounts related to the source or target user by type.

  # Arguments
  - `xtdb-node`: Database con.
  - `type`: Relationship type.
  - `user-id`: User ID of the source or target user.
  - `source?`: True if the user-id is the source user, false if the target user
  # Optional arguments
  - `n`: The number of rows to fetch. Must be an integer greater than 0.
  - `cursor`: A cursor into the table. See note for more details.
  - `reverse`: Reverse the order of the collection (descend instead of ascend).
  - `prev-page`: If true: fetch the previous page instead of the next page.
  - `order-column`: The initial column to order and paginate by. (Ignored if cursor is provided).

  Cursor format: The cursor is a map for the following structure:
  | Key          | Type  | Meaning                                                           |
  |:-------------|:------|:------------------------------------------------------------------|
  | `:cursor-id` | UUID  | ID of the last returned column                                    |
  | `:last`      | Tuple | The paginated column name and last returned value for that column |

  # Return value
  A map of the following structure:
  | Key       | Type       | Meaning                                 | Note                              |
  |:----------|:-----------|:----------------------------------------|-----------------------------------|
  | `:result` | Collection | The result collection of the query      |                                   |
  | `:cursor` | Map        | A cursor to the last item in collection | Absent if the result set is empty |
  "
  [xtdb-node type source-id source? {:keys [n cursor reverse prev-page order-column]
                                     :or {n 10
                                          order-column :at}}]

  (let [;; Asc by default, and reverse and prev-page cancel each other out.
        order-direction (if reverse :desc :asc)

        cursor-dir (if prev-page (if reverse :asc :desc) (if reverse :desc :asc))

        {[cursor-column-name cursor-value] :last :keys [cursor-id]} cursor

        cursor-column-name (or cursor-column-name order-column :at) ; Default value.

        cursor-column (case cursor-column-name
                        :at 'at
                        :nickname 'nickname)

        result
        (xt/q
         xtdb-node
         [(xt/template
           (fn [rel-type rel-user-id cursor-value cursor-id n backwards order-direction no-cursor]
             (->
              (unify
               (from :mushin.db/relationships [state {~(if source? :source :target) rel-user-id
                                                      ~(if source? :target :source) relatee-id
                                                      :type rel-type
                                                      :created-at at
                                                      :xt/id rel-id}])
               (from :mushin.db/users [nickname {:xt/id relatee-id}]))
              (where (or no-cursor
                         (if (or (and (not backwards) (= order-direction :asc))
                                 (and backwards (= order-direction :desc)))
                           (or (> ~cursor-column cursor-value)
                               (and (= ~cursor-column cursor-value) (> rel-id cursor-id)))
                           (or (< ~cursor-column cursor-value)
                               (and (= ~cursor-column cursor-value) (< rel-id cursor-id))))))
              (order-by {:val ~cursor-column
                         :dir ~cursor-dir
                         :nulls :last}
                        {:val rel-id
                         :dir ~cursor-dir})
              (limit ?_4)
              (with {:user {:xt/id relatee-id
                            :nickname nickname}})
              (return rel-id at user ~@(if (stateful? type) ['state] [])))))
          type source-id cursor-value cursor-id n (boolean prev-page) order-direction (not (boolean cursor))])

        {{:keys [nickname]} :user :keys [rel-id at] :as last-row} (last result)]
    (cond-> {:result result}
      (or last-row (< n (count result)))
      (assoc :cursor {:cursor-id rel-id
                      :last [cursor-column-name (case cursor-column-name
                                                  :at at
                                                  :nickname nickname)]}))))


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
  (xt/q
   xtdb-node
   [(xt/template
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


(defn follow-tx
  "Attempts to create a following relationship tx between a follower
  and a followee.

  Return value
  If a following relationship is possible then a transaction is returned, else:
  - `:self-follow`: The follower is trying to follow themself
  - `:following`: The follow is allready following the followee
  - `:denied`: The follower does not have permission to follow the followee"
  [xtdb-node follower-id followee-id]
  (if (= follower-id followee-id)
    :self-follow
    (let [perm (users/check-user-can-view-user xtdb-node follower-id followee-id)]
      (if (or (= perm :allowed)
              (= perm :locked))
        (let [follows? (:state (get-relationship-with xtdb-node :follow follower-id followee-id))]
          (if (or (= :follow-pending follows?)
                  (= :follow-accept follows?))
            :following
            (insert-follow-tx follower-id followee-id (if (= perm :allowed) :follow-accept :follow-pending))))
        :denied))))

(defn unfollow-tx
  "Attempts to create an unfollowing relationship tx between a exfollower
  and an exfollowee.

  Return value
  If an unfollowing relationship is possible then a transaction is
  returned, else:
  - `:self-unfollow`: The ex-follower is trying unfollow themselves
  - `:not-following`: There ex-follower is not following the ex-followee."
  [xtdb-node ex-follower-id ex-followee-id]
  (if (= ex-followee-id ex-follower-id)
    :self-unfollow
    (if-let [follows? (:state (get-relationship-with xtdb-node :follow ex-follower-id ex-followee-id))]
      (case follows?
        (:follow-accept :follow-pending)
        (insert-unfollow-tx ex-follower-id ex-followee-id))
      :not-following)))
