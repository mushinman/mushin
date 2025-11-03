(ns org.mushin.passwords
  (:require [xtdb.api :as xt]
            [buddy.hashers :as hashers]))

(defn nickname-and-password-are-valid?
  "Check that a given password matches a given nickname.

  # Arguments
   - `xtdb-node`: Database.
   - `nickname`: User nickname.
   - `password`: The password for the `nickname`'s user account.

  # Return value
  The user's ID if the password is valid for the given `nickname`, otherwise `false`."
  [xtdb-node nickname password]
  (let [{:keys [password-hash xt/id]}
        (first (xt/q xtdb-node (xt/template (-> (from :mushin.db/users [{:nickname ~nickname} password-hash xt/id])
                                                (limit 1)))))]
    (if (and password-hash (:valid (hashers/verify password password-hash)))
      id
      false)))
