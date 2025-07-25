(ns org.mushin.web.controllers.auth
  (:require [org.mushin.web.auth-utils :refer [failed-auth! check-basic-auth!]]
            [ring.util.http-response :refer [bad-request ok]]
            [clojure.string :as cstr]
            [org.mushin.utils :as utils]))

(defn login! [{:keys [xtdb-node]}
             {:keys [headers session] :as req}]
  (when-not (get headers "authorization")
    (failed-auth! {:error "missing authorization"
                   :message "please authentication using one of our supported schemas"}))
  (let [[auth-type auth-arg] (cstr/split (get headers "authorization") #"\s+")]
    (if (utils/icase-comp auth-type "Basic")
      (->  (ok {:message "Logged in"})
           (assoc :session (assoc session :user-id (:user-id (check-basic-auth! auth-arg xtdb-node)))))
      (bad-request {:error "invalid_request" :message "Malformed authorization header"}))))


(defn logout! [{:keys [session] :as req}]
  (assoc req :session (dissoc session :user-id)))
