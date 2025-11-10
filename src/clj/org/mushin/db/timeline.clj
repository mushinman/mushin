(ns org.mushin.db.timeline
  (:require [xtdb.api :as xt]
            [org.mushin.db.util :as db-util]
            [clj-uuid :as uuid]
            [java-time.api :as jt]
            [org.mushin.db.timestamps :as timestamps]))

(def ^:private base-event-schema
  "Base schema that all timeline events have."
  [:map
   [:xt/id      :uuid]
   [:type       [:enum :pub :repub]]
   timestamps/created-at
   [:obj-id     :uuid]
   [:actor-id   :uuid]])

(def timeline-schema
  "Schema for a timeline of user actions."
  {:mushin.db/timeline
   [:multi :type
    [:pub base-event-schema]
    [:repub base-event-schema]]})

(defn repub-doc
  "Create a republish doc for a given user ID and status ID."
  [user-id status-id]
  {:xt/id      (uuid/v7)
   :type       :repub
   :created-at (jt/zoned-date-time)
   :user-id    user-id
   :status-id  status-id})


(defn pub-doc
  "Create a publish doc for a given user ID and status ID."
  [user-id status-id]
  {:xt/id       (uuid/v7)
   :type        :pub
   :user-id     user-id
   :created-at  (jt/zoned-date-time)
   :status-id   status-id})

(defn get-user-events
  "Create a query that gets a list of events joined with the result object of the event."
  [user-id & {:keys [n offset-by valid-for system-valid-for]
              :or   {n 10
                     offset-by 0}}]
  [(xt/template
    (fn [creator-id]
      (-> (from :mushin.db/timeline
                ~(db-util/query-bind '[obj-id type xt/id created-at {:actor-id creator-id}]
                                     valid-for system-valid-for)))
      (order-by created-at)
      (offset ~offset-by)
      (limit ~n)
      (with {:obj (cond (or (= type :repub) (= type :pub)) (pull (from :mushin.db/statuses [* {:xt/id obj-id}])))})))
   user-id])
