(ns org.mushin.db.relationship
  (:require [org.mushin.db.timestamps :as ts]
            [xtdb.api :as xt]
            [malli.core :as malli]
            [clj-uuid :as uuid]
            [java-time.api :as jt]
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

(defn follow-doc
  [follower followee]
  {:xt/id [:follow follower followee]
   :source follower
   :target followee
   :type :follow
   :created-at (jt/zoned-date-time)
   :state :follow-pending})

(defn relationship-doc
  [source target type]
  {:xt/id (uuid/v7)
   :source source
   :target target
   :created-at (jt/zoned-date-time)
   :type type})

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
