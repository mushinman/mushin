(ns org.mushin.web.controllers.auth
  (:require [org.mushin.web.auth-utils :refer [failed-auth! check-basic-auth!]]
            [ring.util.http-response :refer [bad-request! ok]]
            [clojure.string :as cstr]
            [clojure.tools.logging :as log]
            [org.mushin.utils :as utils]))

(defn login! [{:keys [xtdb-node]}
              {:keys [headers session]}]
  (when-not (get headers "authorization")
    (failed-auth! {:error "missing authorization"
                   :message "please authentication using one of our supported schemas"}))
    (let [[auth-type auth-arg] (cstr/split (get headers "authorization") #"\s+")
          user-id (if (utils/icase-comp auth-type "Basic")
                     (:user-id (check-basic-auth! auth-arg xtdb-node))
                     (bad-request! {:error "invalid_request" :message "Malformed authorization header"}))]
      (log/info "Successfully logged in user" {:event :logged-in :user-id user-id})
      (->  (ok {:message "Logged in"})
           (assoc :session (assoc session :user-id user-id)))))


(defn logout! [{session :session}]
  (log/info "Logging out user " {:event :logged-out :user-id (:user-id session)})
  (-> (ok {:message "Logged out"})
      (assoc :session (dissoc session :user-id))))
