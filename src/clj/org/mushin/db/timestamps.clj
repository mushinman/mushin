(ns org.mushin.db.timestamps
  (:require [malli.experimental.time :as mallt]))

(def created-at
  [:created-at (mallt/-zoned-date-time-schema)])

(def updated-at
  [:updated-at (mallt/-zoned-date-time-schema)])
