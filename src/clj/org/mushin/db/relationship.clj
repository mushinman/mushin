(ns org.mushin.db.relationship
  (:require [org.mushin.db.timestamps :as ts]
            [xtdb.api :as xt]
            [malli.core :as malli]
            [clj-uuid :as uuid]
            [java-time.api :as jt]
            [org.mushin.db.util :as db]))


(def ^:private relationship-types-schema
  [:enum :follow :block :mute])

(def ^:private follow-states-schema
  [:enum :follow-pending :follow-accept :follow-reject :unfollow])

;; Currently XTDB only supports UUIDs, ints, strings, and keywords as xt/ids,
;; but if they ever support tuples we should condense this schema into this:
;; [:xt/id [:tuple :keyword :uuid :uuid]], where keyword is the relationship type,
;; and the UUIDs are the IDs of the source and target users respectively.
;; This would also get rid of the requirement to ASSERT NOT EXIST when we
;; create a new relationship row.
(def ^:private base-realtionship-schema
  [:map
   [:xt/id                   :uuid]
   [:source                  :uuid]
   [:target                  :uuid]
   [:type                    :keyword]
   ts/created-at])

(def relationship-schema
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
  [doc]
  (db/insert-unless-exists-tx
   :mushin.db/relationships
   doc
   :type :source :target))
