(ns org.mushin.db.timeline
  (:require [xtdb.api :as xt]
            [org.mushin.db.util :as db-util]
            [clj-uuid :as uuid]
            [java-time.api :as jt]
            [org.mushin.db.timestamps :as timestamps]))

(def timeline-schema
  "Base schema that all timeline events have."
  {:mushin.db/timeline
   [:map
    [:xt/id      :uuid]
    [:type       [:enum :pub :repub]]
    timestamps/created-at
    [:obj-id     :uuid]
    [:actor-id   :uuid]]})


(defn create-repub-doc
  "Create a republish doc for a given user ID and status ID."
  [user-id status-id]
  {:xt/id      (uuid/v7)
   :type       :repub
   :created-at (jt/zoned-date-time)
   :actor-id    user-id
   :obj-id  status-id})


(defn create-pub-doc
  "Create a publish doc for a given user ID and status ID."
  [user-id status-id]
  {:xt/id       (uuid/v7)
   :type        :pub
   :actor-id     user-id
   :created-at  (jt/zoned-date-time)
   :obj-id   status-id})

(defn insert-timeline-doc
  [timeline-doc]
  [:put-docs :mushin.db/timeline timeline-doc])

(defn get-user-events
  "Create a query that gets a list of events joined with the result object of the event."
  [db-con user-id & {:keys [n offset-by]
                     :or   {n 10
                            offset-by 0}}]
  (xt/q
   db-con
   [(xt/template
     (fn [creator-id offset-by n]
       (->
        (from :mushin.db/timeline [obj-id type xt/id created-at {:actor-id creator-id}])
        (order-by {:val created-at, :dir :asc, :nulls :last}
                  xt/id)
        (offset offset-by)
        (limit n)
        (with {:obj (cond (or (= type :repub) (= type :pub))
                          (pull (from :mushin.db/statuses [{:xt/id status-id} creator])
                                {:args [{:status-id obj-id}]}))}))))
    user-id offset-by n]))
