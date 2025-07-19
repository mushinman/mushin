(ns org.mushin.schema.db.users
  (:require [malli.experimental.time :as mallt]))

(def schema
  {::tiny-string  [:string {:min 1 :max 32}]
   ::short-string [:string {:min 1 :max 256}]
   ::long-string  [:string {:min 1 :max 4096}]

   :mushin.db/users [:map {:closed true}
                     [:xt/id     :uuid]
                     [:email     [:and ::short-string [:re #".+@.+"]]]
                     [:nickname  [:and ::tiny-string [:re #"\w+"]]]
                     [:password-hash :string]
                     [:joined-at (mallt/-zoned-date-time-schema)]
                     [:last-logged-in-at (mallt/-zoned-date-time-schema)]]})
