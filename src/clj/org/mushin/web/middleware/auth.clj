(ns org.mushin.web.middleware.auth
  (:require
   [ring.util.http-response :refer [bad-request!]]
   [clojure.tools.logging :as log]
   [org.mushin.web.auth-utils :refer [failed-auth! invalid-auth! check-basic-auth!]]
   [org.mushin.utils :as utils]
   [clojure.string :as cstr]))


;; TODO probably have multiple ways to store the tokens, e.g. db, in-memory, some external service etc..
(defn check-bearer
  ;; TODO finish this
  [xtdb-node auth-arg]
  (when-not auth-arg
    (invalid-auth! {:error "invalid_basic" :message "the provided bearer authorization header had no credentials"})))

(defn wrap-optional-authenticate-user
  [{:keys [xtdb-node]} handler]
  (fn [{{:keys [user-id]} :session :keys [headers] :as req}]
    (handler
     (cond
       user-id req

       (nil? (get headers "authorization"))
       req

       :else
       (let [[auth-type auth-arg] (cstr/split (get headers "authorization") #"\s+")]
         (assoc-in req [:session :user-id]
                   (cond
                     ;;(utils/icase-comp auth-type "Bearer") (check-bearer auth-arg xtdb-node)
                     (utils/icase-comp auth-type "Basic")
                     (check-basic-auth! xtdb-node auth-arg)

                     :else
                     (bad-request! {:error :invalid-request :message "Malformed authorization header"}))))))))

(defn wrap-authenticate-user
  [{:keys [xtdb-node]} handler]
  (fn [{{:keys [user-id]} :session :keys [headers] :as req}]
    (handler
     (cond
       user-id req

       (nil? (get headers "authorization"))
       (failed-auth! {:error "missing authorization"
                      :message "please authenticate using one of our supported schemas"})

       :else
       (let [[auth-type auth-arg] (cstr/split (get headers "authorization") #"\s+")]
         (assoc-in req [:session :user-id]
                   (cond
                     ;;(utils/icase-comp auth-type "Bearer") (check-bearer auth-arg xtdb-node)
                     (utils/icase-comp auth-type "Basic")
                     (check-basic-auth! xtdb-node auth-arg)

                     :else
                     (bad-request! {:error :invalid-request :message "Malformed authorization header"}))))))))
