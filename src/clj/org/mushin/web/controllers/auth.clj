(ns org.mushin.web.controllers.auth
  (:require [org.mushin.web.auth-utils :refer [failed-auth! check-basic-auth!]]
            [ring.util.http-response :refer [bad-request! ok unauthorized!]]
            [clojure.string :as cstr]
            [org.mushin.db.remember-me :as remember-me]
            [org.mushin.db.util :as db]
            [xtdb.api :as xt]
            [clojure.tools.logging :as log]
            [org.mushin.utils :as utils]))

(defn login! [{:keys [xtdb-node]}
              {:keys [headers session]}]
  (when-not (get headers "authorization")
    (failed-auth! {:error "missing authorization"
                   :message "please authenticate using one of our supported schemas"}))
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

;; The schema of a remember me cookie:
;; [:user user id]
;; {:}

(defn refresh-session!
  "Refreshes user session based on a `remember-me` cookie.

  Checks the validity of the incoming cookie. If valid: logs
  the user and creates a new session, and awards the user a
  new remember me cookie. If invalid: returns `unauthorized`.

  ## Arguments
  * `:xtdb-node` - xtdb
  * `:cookies` - HTTP request cookies

  ## Returns
  * HTTP 200 on success with new session and remember me cookies

  ## Throws
  * HTTP exception with code `unauthorized` on invalid cookie.
  "
  [{:keys [xtdb-node]}
   {:keys [cookies session]}]
  (if-let [cookie-value (get-in cookies ["remember-me" :value])]
    (if-let [selector-validator (cstr/split cookie-value #":")]
      (if (= (count selector-validator) 2)
        (if-let [user-id (remember-me/recall-user! xtdb-node (first selector-validator) (second selector-validator))]
          ;; Recall-user destroyed their last token, so we must allocate them a new one.
          (do
            (log/info "Allocating new session" {:event :user-logged-in :user-id user-id :method :remember-me})
            (->  (ok {:message "Logged in"})
                 (assoc :session (assoc session :user-id user-id))))
          (unauthorized! {:error :invalid-remember-me :message "Your remember me cookie is invalid3"}))
        (unauthorized! {:error :invalid-remember-me :message "Your remember me cookie is invalid2"}))
      (unauthorized! {:error :invalid-remember-me :message "Your remember me cookie is invalid1"}))
    (unauthorized! {:error :no-remember-me :message "Your request was missing a remember me cookie"})))
