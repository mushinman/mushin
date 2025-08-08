(ns org.mushin.db.remember-me
  (:require [malli.experimental.time :as mallt]
            [buddy.core.nonce :as nonce]
            [java-time.api :as time]
            [xtdb.api :as xt]
            [clj-commons.digest :as digest]
            [honey.sql :as sql]
            [org.mushin.tokens :as tokens]
            [org.mushin.db.util :as db]
            [buddy.core.codecs :as codecs])
  (:import [java.security MessageDigest]))

(def purge-invalid-tokens-query
  [:sql (first (sql/format {:erase-from [:mushin.db/remember-me]
                            :where [:> [:+ :created-at :valid-for] [:now]]}))])

;; TODO should probably change valid-for's type from duration to period.
(def remember-me
  {:mushin.db/remember-me [:map {:closed true}
                           [:xt/id                   :uuid]
                           [:user                    :uuid]
                           [:selector                :string]
                           [:hashed-validator        :string]
                           [:created-at              (mallt/-zoned-date-time-schema)]
                           [:valid-for               :time/duration]]})

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
   (let [selector (tokens/generate-token 16)
         validator (tokens/generate-token 32)]
     {:selector selector
      :validator validator
      :doc {:xt/id            (random-uuid)
            :selector         selector
            :hashed-validator (codecs/bytes->b64u (codecs/b64->bytes (digest/sha-256 validator)))
            :user             user-id
            :created-at       (time/zoned-date-time)
            :valid-for        valid-for}}))
  ([user-id] (remember-user user-id (time/duration 30 :days))))

(defn recall-user!
  [xtdb-node selector validator]
  (when-let [token (first (xt/q xtdb-node (xt/template (-> (from :mushin.db/remember-me [created-at valid-for user hashed-validator xt/id {:selector ~selector}])
                                                           (where (< ~(time/zoned-date-time)
                                                                     (+ created-at valid-for)))
                                                           (limit 1)
                                                           (return hashed-validator user xt/id)))))]
    (when (MessageDigest/isEqual (codecs/b64u->bytes (:hashed-validator token)) (codecs/b64->bytes (digest/sha-256 validator)))
      ;; Delete the token.
      ;; TODO fix db/submit-tx not working for erase-docs, delete-docs
      (xt/submit-tx xtdb-node [[:erase-docs :mushin.db/remember-me (:xt/id token)]])
      (:user token))))

