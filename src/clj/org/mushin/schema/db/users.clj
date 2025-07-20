(ns org.mushin.schema.db.users
  (:require [malli.experimental.time :as mallt]
            [org.mushin.schema.users :as schemas]))

(def schema
  {::tiny-string  [:string {:min 1 :max 32}]
   ::short-string [:string {:min 1 :max 256}]
   ::long-string  [:string {:min 1 :max 4096}]

   :mushin.db/users [:map
                     [:xt/id     :uuid]
                     [:email  {:optional true}  [:and ::short-string [:re #".+@.+"]]]
                     schemas/user-schema
                     [:password-hash :string]
                     [:joined-at (mallt/-zoned-date-time-schema)]
                     [:last-logged-in-at (mallt/-zoned-date-time-schema)]]})
