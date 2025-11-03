(ns org.mushin.db.remember-me
  (:require [malli.experimental.time :as mallt]
            [java-time.api :as time]
            [xtdb.api :as xt]
            [clj-uuid :as uuid]
            [org.mushin.digest :as digest]
            [honey.sql :as sql]
            [org.mushin.tokens :as tokens]
            [org.mushin.codecs :as codecs]))

(def purge-invalid-tokens-query
  "XTDB transaction part to purge all invalid tokens."
  [:sql (-> {:erase-from [:mushin.db/remember-me]
             :where [:> [:+ :created-at :valid-for] [:now]]}
            sql/format
            first)])


(def forget-everybody
  "XTDB transaction part to purge all tokens."
  [:sql (first (sql/format {:erase-from [:mushin.db/remember-me]}))])

(def remember-me
  {:mushin.db/remember-me
   [:map {:closed true}
    [:xt/id                   :uuid]
    [:user                    :uuid]
    [:selector                :string]
    [:hashed-validator        :string]
    [:created-at              (mallt/-zoned-date-time-schema)]
    [:valid-for               :time/period]]})

(defn get-token-value
  "Get the value of a token from the token string.

  ## Arguments
  * `xtdb-node` - xtdb
  * `token` - Token string

  ## Returns
  * `nil` if there is no token matching `token` in the database.
  * An empty map if there is a token matching `token` in the database, but there is no value associated with it.
  * The value, which is always a map, if there is a matching token in the database and if there is a value associated with it.
  "
  [xtdb-node token]
  (let [value (first (xt/q xtdb-node (xt/template (-> (from :mushin.db/remember-me [created-at valid-for value {:token ~token}])
                                                      (where (< ~(time/zoned-date-time)
                                                                (+ created-at valid-for)))
                                                      (limit 1)
                                                      (return value)))))]
    (when value
      (or (:value value) value))))

(defn remember-user
  ([user-id valid-for]
   (let [selector (codecs/bytes->b64u (tokens/generate-token 16))
         validator (tokens/generate-token 32)]
     {:selector selector
      :validator (codecs/bytes->b64u validator)
      :valid-for        valid-for
      :doc {:xt/id            (uuid/v7)
            :selector         selector
            :hashed-validator (digest/sha-256-b64u validator)
            :user             user-id
            :created-at       (time/zoned-date-time)
            :valid-for        valid-for}}))
  ([user-id] (remember-user user-id (time/period 30 :days))))


(defn recall-user
  [xtdb-node selector validator]
  (when-let [token
             (first
              (xt/q
               xtdb-node
               (xt/template (-> (from :mushin.db/remember-me [created-at valid-for user hashed-validator xt/id {:selector ~selector}])
                                (where (< ~(time/zoned-date-time)
                                          (+ created-at valid-for)))
                                (limit 1)
                                (return hashed-validator user xt/id)))))]
    (when (digest/eq (codecs/b64u->bytes (:hashed-validator token)) (digest/sha-256-bytes (codecs/b64u->bytes validator)))
      (dissoc token :hashed-validator))))
