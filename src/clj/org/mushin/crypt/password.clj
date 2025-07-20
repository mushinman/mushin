(ns org.mushin.crypt.password
  (:require [buddy.hashers :as hashers]))

(defn verify [attempt password-hash]
  (hashers/verify attempt password-hash))

(defn hash-password [password]
  (hashers/derive password {:alg :argon2id}))
