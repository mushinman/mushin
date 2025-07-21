(ns org.mushin.schema.db.resources
  (:require [org.mushin.schema.db.timestamps :as timestamps])
  (:import [java.net URI]))

(defn valid-uri? [s]
  (try
    (URI. s)
    true
    (catch Exception _
      false)))

(def resources-schema
  {:mushin.db/resources
   [:map {:closed true}
    [:xt/id      :uuid]
    timestamps/created-at
    [:checksum    :string]
    [:mime-type  :string]
    [:location   [:and :string [:fn valid-uri?]]]]})
