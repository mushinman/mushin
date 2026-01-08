(ns org.mushin.db.resource-meta
  (:require [malli.experimental.time :as mallt]
            [java-time.api :as time]
            [org.mushin.utils :refer [to-java-uri]]
            [org.mushin.db.util :as db-util]
            [xtdb.api :as xt]
            [clj-uuid :as uuid]
            [org.mushin.digest :as digest]
            [honey.sql :as sql]
            [org.mushin.tokens :as tokens]
            [org.mushin.codecs :as codecs]))

(def resource-meta-schema
  {:mushin.db/resource-meta
   [:map {:closed true}
    [:xt/id                   :string]
    [:location                'uri?]
    [:mime-type               :string]
    [:created-at              (mallt/-zoned-date-time-schema)]]})

(defn create-resource-meta-doc
  [name location mime-type]
  {:xt/id name 
   :location (to-java-uri location)
   :mime-type mime-type
   :created-at (time/zoned-date-time)})

(defn insert-resource-tx
  [doc]
  (db-util/insert-unless-exists-tx
   :mushin.db/resource-meta
   doc
   [:location]))

(defn get-resource-by-id
  [con id]
  (->
   (xt/q con [(xt/template
               (fn [id]
                 (from :mushin.db/resource-meta [* {:xt/id id}])
                 (limit 1)))
              id])
   first))

(defn resource-exists?
  [db-con id]
  (db-util/record-exists? db-con :mushin.db/resource-meta id))

(defn delete-resource-meta-tx
  [name]
  [:delete-docs :mushin.db/resource-meta name])

