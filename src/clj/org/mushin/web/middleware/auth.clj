(ns org.mushin.web.middleware.auth
  (:require
   [ring.util.http-response :refer [unauthorized! bad-request!]]
   [ring.util.codec :as ring-codec]
   [xtdb.api :as xt]
   [org.mushin.utils :as utils]
   [clojure.string :as cstr]
   [buddy.hashers :as bhash]))

;; TODO There will eventually be two realms at least: Admin Visible and User Visible

(defn- challenge-headers
  []
  {"www-authenticate" ["Basic realm=\"User Visible Realm\", charset=\"UTF-8\""
                       "Bearer realm=\"User Visible Realm\", error=\"no_token\", error=\"The user did not provide an authorization token\""]})

(defn- failed-auth!
  [body]
  (-> (unauthorized! body)
      (assoc :headers (challenge-headers))))

(defn- invalid-auth!
  [body]
  (-> (bad-request! body)
      (assoc :headers (challenge-headers))))

;; TODO probably have multiple ways to store the tokens, e.g. db, in-memory, some external service etc..
(defn check-bearer [auth-arg xtdb-node]
  (when-not auth-arg
    (invalid-auth! {:error "invalid_basic" :message "the provided bearer authorization header had no credentials"})))


(defn check-basic-auth [auth-arg xtdb-node]
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
      (if-not (and password-hash (:valid (bhash/verify password-attempt password-hash)))
        (failed-auth! {:error "wrong_username_or_password" :message "the provided username or password is incorrect"})
        {:user-id id}))))

(defn wrap-authenticate-user [{:keys [xtdb-node]} handler]
  (fn [{:keys [headers] :as req}]
    (if (nil? (get headers "authorization"))
      (failed-auth! {:error "missing authorization"
                     :message "please authentication using one of our supported schemas"})
      (let [[auth-type auth-arg] (cstr/split (get headers "authorization") #"\s+")]
        (->
         (cond
           (utils/icase-comp auth-type "Bearer") (check-bearer auth-arg xtdb-node)
           (utils/icase-comp auth-type "Basic") (check-basic-auth auth-arg xtdb-node)
           :else (bad-request! {:error "invalid_request" :message "Malformed authorization header"}))
         (merge req)
         (handler))))))
