(ns org.mushin.db.statuses
  (:require [org.mushin.db.util :as db]
            [java-time.api :as jt]
            [xtdb.api :as xt]
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

(defn get-status-by-id
  ([xtdb-node cols id] (db/lookup-by-id xtdb-node :mushin.db/statuses cols id))
  ([xtdb-node id] (get-status-by-id xtdb-node '[*] id)))

(defn get-n-statuses-by-user
  ([xtdb-node user-id n offset] (get-n-statuses-by-user xtdb-node user-id n offset :desc))
  ([xtdb-node user-id n offset direction]
  (xt/q xtdb-node (xt/template (-> (from :mushin.db/statuses [* {:user ~user-id}])
                                   (order-by {:val created-at, :dir ~direction})
                                   (offset ~offset)
                                   (limit ~n))))))

(defn get-n-status-ids-by-user
  ([xtdb-node user-id n offset] (get-n-status-ids-by-user xtdb-node user-id n offset :desc))
  ([xtdb-node user-id n offset direction]
   (xt/q xtdb-node (xt/template (-> (from :mushin.db/statuses [xt/id created-at {:user ~user-id}])
                                    (order-by {:val created-at, :dir ~direction})
                                    (without created-at)
                                    (offset ~offset)
                                    (limit ~n))))))
