(ns org.mushin.web.middleware.auth
  (:require
   [ring.util.http-response :refer [bad-request!]]
   [org.mushin.web.auth-utils :refer [failed-auth! invalid-auth! check-basic-auth!]]
   [org.mushin.utils :as utils]
   [clojure.string :as cstr]))


;; TODO probably have multiple ways to store the tokens, e.g. db, in-memory, some external service etc..
;; TODO implement.
(defn check-bearer [auth-arg xtdb-node]
  (when-not auth-arg
    (invalid-auth! {:error "invalid_basic" :message "the provided bearer authorization header had no credentials"})))

(defn wrap-authenticate-user [{:keys [xtdb-node]} handler]
  (fn [{:keys [headers] :as req}]
    (if (nil? (get headers "authorization"))
      (failed-auth! {:error "missing authorization"
                     :message "please authenticate using one of our supported schemas"})
      (let [[auth-type auth-arg] (cstr/split (get headers "authorization") #"\s+")]
        (->
         (cond
           ;(utils/icase-comp auth-type "Bearer") (check-bearer auth-arg xtdb-node)
           (utils/icase-comp auth-type "Basic") (check-basic-auth! auth-arg xtdb-node)
           :else (bad-request! {:error "invalid_request" :message "Malformed authorization header"}))
         (merge req)
         (handler))))))
