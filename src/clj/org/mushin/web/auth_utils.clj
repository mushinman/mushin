(ns org.mushin.web.auth-utils
  (:require [ring.util.http-response :refer [unauthorized! bad-request!]]
            [ring.util.codec :as ring-codec]
            [clojure.string :as cstr]
            [xtdb.api :as xt]
            [buddy.hashers :as hasher]))


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

(defn check-basic-auth! [auth-arg xtdb-node]
  (when-not auth-arg
    (invalid-auth! {:error "invalid_basic" :message "the provided basic authorization header had no credentials"}))

  (let [b64-creds (try
                    (String. (ring-codec/base64-decode auth-arg) (java.nio.charset.Charset/forName "UTF-8"))
                    (catch Exception _
                      (invalid-auth! {:error "invalid_base64" :message "the basic authorization header contained invalid base64"})))
        [username password-attempt :as creds] (cstr/split b64-creds #":")]

    (when (some nil? creds)
      (invalid-auth! {:error "invalid_basic" :message "the provided basic authorization header was not in the standard user:password format"}))

    (let [{:keys [password-hash xt/id]} (first (xt/q xtdb-node (xt/template (-> (from :mushin.db/users [{:nickname ~username} password-hash xt/id])
                                                                                (limit 1)))))]
      (if-not (and password-hash (:valid (hasher/verify password-attempt password-hash)))
        (failed-auth! {:error "wrong_username_or_password" :message "the provided username or password is incorrect"})
        {:user-id id}))))

(defn user-has-permissions-for?
  "Determine if the user `subject-user-id` has the permissions to perform actions on user `object-user-id`."
  [subject-user-id object-user-id]
  ;; TODO this is definately going to need to be expanded.
  (= subject-user-id object-user-id))

(defn user-has-permissions-for!
  "Determine if the user `subject-user-id` has the permissions to perform actions on user `object-user-id`."
  [subject-user-id object-user-id]
  ;; TODO this is definately going to need to be expanded.
  (or (user-has-permissions-for? subject-user-id object-user-id) (unauthorized! {:error "insufficient_permission" :message "User does not have permissions for that action"})))
