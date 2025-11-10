(ns org.mushin.db.likes
  (:require [org.mushin.db.util :as db]
            [java-time.api :as jt]
            [honey.sql.helpers :as h]
            [honey.sql :as sql]
            [org.mushin.db.authorization :as authz]
            [clj-uuid :as uuid]
            [xtdb.api :as xt]
            [org.mushin.db.timestamps :as timestamps]
            [java-time.api :as time]))


(def likes-schema
  "Schema for status likes."
  {:mushin.db/likes
   [:map
    [:xt/id                  :uuid]
    [:actor-id               :uuid]
    [:status-id              :uuid]
    timestamps/created-at]})

(defn like-doc
  [actor-id status-id]
  {:xt/id (uuid/v4)
   :actor-id actor-id
   :status-id status-id
   :created-at (time/zoned-date-time)})

(defn status-likes
  [status-id & {:keys [n offset-by valid-for system-valid-for]
                :or   {n 10
                       offset-by 0}}]
  [(xt/template
    (fn [status-id]
      (->
       (from :mushin.db/likes ~(db/query-bind '[* {:status-id status-id}]
                                              valid-for system-valid-for))
       (offset ~offset-by)
       (limit ~n))))
   status-id])

(defn likes-by-actor
  [actor-id & {:keys [n offset-by valid-for system-valid-for]
               :or   {n 10
                      offset-by 0}}]
  [(xt/template
    (fn [actor-id]
      (->
       (from :mushin.db/likes ~(db/query-bind '[* {:actor-id actor-id}]
                                              valid-for system-valid-for))
       (offset ~offset-by)
       (limit ~n))))
   actor-id])

(defn likes-by-id
  [like-id & {:keys [n offset-by valid-for system-valid-for]
              :or   {n 10
                     offset-by 0}}]
  [(xt/template
    (fn [like-id]
      (->
       (from :mushin.db/likes ~(db/query-bind '[* {:xt/id like-id}]
                                              valid-for system-valid-for))
       (offset ~offset-by)
       (limit 1))))
   like-id])

(defn remove-like-tx
  [])
