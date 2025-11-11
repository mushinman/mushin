(ns org.mushin.web.auth-utils
  (:require [ring.util.http-response :refer [unauthorized! bad-request!]]
            [ring.util.codec :as ring-codec]
            [clojure.tools.logging :as log]
            [clojure.string :as cstr]
            [org.mushin.db.users :as users]))


;; TODO There will eventually be two realms at least: Admin Visible and User Visible

(defn challenge-headers
  []
  ; Add this to the vector when we support bearers: Bearer realm=\"User Visible Realm\", error=\"no_token\", error=\"The user did not provide an authorization token\"
  {"www-authenticate" ["Basic realm=\"User Visible Realm\", charset=\"UTF-8\""]})

(defn failed-auth!
  [body]
  (-> (unauthorized! body)
      (assoc :headers (challenge-headers))))

(defn invalid-auth!
  [body]
  (-> (bad-request! body)
      (assoc :headers (challenge-headers))))

(defn check-basic-auth!
  "Validate a basic authentication header value.

  # Arguments
   - `xtdb-node`: Databse.
   - `auth-arg`: Basic auth string (just the base64 pair).

  # Return value
  A map containing a key `:user-id` with the user's ID on success.

  Will throw an HTTP exception if the input format is not correct, or the nickname/password pair failed validation."
  [xtdb-node auth-arg]
  (when-not auth-arg
    (invalid-auth! {:error "invalid_basic" :message "the provided basic authorization header had no credentials"}))

  (let [b64-creds (try
                    (String. (ring-codec/base64-decode auth-arg) (java.nio.charset.Charset/forName "UTF-8"))
                    (catch Exception _
                      (invalid-auth! {:error "invalid_base64" :message "The basic authorization header contained invalid base64"})))
        [nickname password-attempt :as creds] (cstr/split b64-creds #":")]

    (when (some nil? creds)
      (invalid-auth! {:error :invalid-basic-auth :message "The provided basic authorization header was not in the standard user:password format"}))
    (let [id (users/can-login? xtdb-node nickname password-attempt)]
      (if (uuid? id)
        id
        (failed-auth! {:error id})))))

(defn user-has-permissions-for?
  "Determine if the user `subject-user-id` has the permissions to perform actions on user `object-user-id`."
  [subject-user-id object-user-id]
  ;; TODO this is definately going to need to be expanded.
  (= subject-user-id object-user-id))

(defn user-has-permissions-for!
  "Determine if the user `subject-user-id` has the permissions to perform actions on user `object-user-id`."
  [subject-user-id object-user-id]
  ;; TODO this is definately going to need to be expanded.
  (or (user-has-permissions-for? subject-user-id object-user-id) (unauthorized! {:error :insufficient-permission
                                                                                 :message "User does not have permissions for that action"})))
(defn user-blocked
  "Response body for authorization failure from being blocked."
  [target-id]
  {:error :blocked-by-user
   :message "You have been blocked by that user"
   :blocked-by target-id})
