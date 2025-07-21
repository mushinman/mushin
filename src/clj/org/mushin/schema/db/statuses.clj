(ns org.mushin.schema.db.statuses
  (:require [org.mushin.schema.statuses :as schemas]
            [org.mushin.schema.db.timestamps :as timestamps]))

(def statuses-schema
  {:mushin.db/statuses
   [:map
    [:xt/id      :uuid]
    [:user       :uuid]
    timestamps/created-at
    timestamps/updated-at
    [:tags       [:vector :string]]
    [:content    [:or [:map schemas/text-status-schema]
                  [:map
                   [:image :uuid]
                   [:text :string]]]]]})
