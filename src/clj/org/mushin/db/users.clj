(ns org.mushin.db.users
  (:require [malli.experimental.time :as mallt]))

(def schema
  {::short-string [:string {:min 1 :max 256}]
   ::long-string  [:string {:min 1 :max 4096}]

   :mushin.db/users [:map {:closed true}
                     [:xt/id     :uuid]
                     [:email     [:and ::short-string [:re #".+@.+"]]]
                     [:joined-at (mallt/-zoned-date-time-schema)]
                     [:last-logged-in-at (mallt/-zoned-date-time-schema)]]})
