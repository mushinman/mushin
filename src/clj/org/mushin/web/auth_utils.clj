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
