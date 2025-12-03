(ns org.mushin.account-name-validator
  (:import [org.mushin.validator AccountIdentifierValidator]))

(def account-validator
  "Account name validator instance."
  (AccountIdentifierValidator.))

(defn valid-account-name?
  "Returns true if `account-name` is a valid account name, false otherwise."
  [^String account-name]
  (.isValidAccountName account-validator account-name))

(defn valid-domain?
  "Returns true if `domain` is a valid domain name, false otherwise."
  [^String domain]
  (.isValidDomain account-validator domain))

(defn valid-account-identifier?
  "Returns true if `acc` is a valid fully qualified account, false otherwise."
  [^String acc]
  (.isValidAccountIdentifier account-validator acc))
