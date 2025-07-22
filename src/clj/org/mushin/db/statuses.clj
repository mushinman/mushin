(ns org.mushin.db.statuses
  (:require [org.mushin.db.util :as db]
            [java-time.api :as jt]
            [org.mushin.db.timestamps :as timestamps]))

(def statuses-schema
  {:mushin.db/statuses
   [:map
    [:xt/id      :uuid]
    [:user       :uuid]
    timestamps/created-at
    timestamps/updated-at
    [:tags       [:vector :string]]
    [:content    [:or [:map [:text :string]]
                  [:map
                   [:image :uuid]
                   [:text :string]]]]]})

(defn create-status! [xtdb-node content user-id]
  (let [now (jt/zoned-date-time)
        doc {:xt/id (random-uuid)
             :user  user-id
             :created-at now
             :updated-at now
             :tags []
             :content content}]
    (db/execute-tx xtdb-node [[:put-docs :mushin.db/statuses
                               doc]])
    doc))
