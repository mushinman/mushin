(ns org.mushin.db.auth
  (:require [malli.experimental.time :as mallt]
            [buddy.core.nonce :as nonce]
            [java-time.api :as jt]
            [buddy.core.codecs :as codecs]))

(def token
  {:mushin.db/tokens [:map {:closed true}
                      [:xt/id           :uuid]
                      [:token           :string]
                      [:created-at      (mallt/-zoned-date-time-schema)]
                      [:valid-until     (mallt/-zoned-date-time-schema)]]})

(defn generate-token-str
  [n-bytes]
  (-> (nonce/random-bytes n-bytes)
      (codecs/bytes->b64-str)))

(defn generate-token
  [n-bytes valid-time]
  (let [now (jt/zoned-date-time)]
    {:xt/id (random-uuid)
     :token (generate-token-str n-bytes)
     :create-at now
     :valid-unti (jt/plus now valid-time)}))
