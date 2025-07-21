(ns org.mushin.db.statuses
  (:require [org.mushin.db.util :as db]
            [java-time.api :as jt]))

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
