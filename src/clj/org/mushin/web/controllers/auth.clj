(ns org.mushin.web.controllers.auth
  (:require [org.mushin.web.auth-utils :refer [failed-auth! check-basic-auth!]]
            [ring.util.http-response :refer [bad-request! ok unauthorized!]]
            [ring.util.response :as resp]
            [clojure.string :as cstr]
            [org.mushin.db.remember-me :as remember-me]
            [org.mushin.db.util :as db]
            [xtdb.api :as xt]
            [java-time.api :as time]
            [clojure.tools.logging :as log]
            [org.mushin.utils :as utils]))

(defn remember-me-cookie
  [response & [selector validator valid-for]]
  (let [value (if (and selector validator)
                (str selector ":" validator)
                "")
        max-age (if valid-for
                  (time/as valid-for :seconds)
                  0)]
    (resp/set-cookie response "remember-me" value
                     {:path "/api/session"
     ;;:domain "example.com" TODO
                      :http-only true
                      :secure true
                      :same-site :strict
                      :max-age max-age})))

(defn login! [{:keys [xtdb-node]}
              {:keys [headers session]}]
  (when-not (get headers "authorization")
    (failed-auth! {:error "missing authorization"
                   :message "please authenticate using one of our supported schemas"}))

  (let [[auth-type auth-arg] (cstr/split (get headers "authorization") #"\s+")
        user-id (if (utils/icase-comp auth-type "Basic")
                  (:user-id (check-basic-auth! auth-arg xtdb-node))
                  (bad-request! {:error "invalid_request" :message "Malformed authorization header"}))
        {:keys [doc selector validator valid-for]} (remember-me/remember-user user-id)]

    (log/info "Successfully logged in user" {:event :logged-in :user-id user-id})
    (db/submit-tx xtdb-node [[:put-docs :mushin.db/remember-me doc]])
    (-> (ok {:message "Logged in"})
        (assoc :session (assoc session :user-id user-id))
        (remember-me-cookie selector validator valid-for))))

(defn logout! [{xtdb-node :xtdb-node}
               {session :session cookies :cookies}]
  (log/info "Logging out user " {:event :logged-out :user-id (:user-id session)})
  (when-let [cookie-value (get-in cookies ["remember-me" :value])]
    ;; Delete any remember-me cookies that were submitted.
    (when-let [selector-validator (cstr/split cookie-value #":")]
      (when (= (count selector-validator) 2)
        (when-let [cookie (remember-me/recall-user xtdb-node (first selector-validator) (second selector-validator))]
          (db/submit-tx xtdb-node [[:erase-docs :mushin.db/remember-me (:xt/id cookie)]])))))
  (-> (ok {:message "Logged out"})
      (assoc :session (dissoc session :user-id))
      (remember-me-cookie)))

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
        (if-let [cookie (remember-me/recall-user xtdb-node (first selector-validator) (second selector-validator))]
          ;; Recall-user destroyed their last token, so we must allocate them a new one.
          ;;
          (let [{:keys [user-id xt/id]} cookie
                {:keys [selector validator doc valid-for]} (remember-me/remember-user user-id)]
            (log/info "Allocating new session" {:event :user-logged-in :user-id user-id :method :remember-me})
            (xt/submit-tx xtdb-node [[:erase-docs :mushin.db/remember-me id]
                                     [:put-docs :mushin.db/remember-me doc]])
            (->  (ok {:message "Logged in"})
                 (assoc :session (assoc session :user-id user-id))
                 (remember-me-cookie selector validator valid-for)))
          (unauthorized! {:error :invalid-remember-me :message "Your remember me cookie is invalid"}))
        (unauthorized! {:error :invalid-remember-me :message "Your remember me cookie is invalid"}))
      (unauthorized! {:error :invalid-remember-me :message "Your remember me cookie is invalid"}))
    (unauthorized! {:error :no-remember-me :message "Your request was missing a remember me cookie"})))
