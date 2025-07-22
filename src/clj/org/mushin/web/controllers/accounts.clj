(ns org.mushin.web.controllers.accounts
  (:require
            [org.mushin.db.users :as db-users]
            [ring.util.http-response :refer [conflict! created]]
            [clojure.tools.logging :as log]))

(def create-account-body
  [:map
   [:email {:optional true} [:and :string [:re #".+@.+"]]]
   [:password [:string {:min 8 :max 128}]]
   db-users/user-schema
   ; TODO recapcha
   ])

(defn create-account-post!
  [{:keys [xtdb-node]}
   {{{:keys [nickname password]} :body} :parameters}]
  (when (db-users/check-user-exists-by-name? xtdb-node nickname)
    (log/info {:event :creating-user-failed :nickname nickname :reason :user-already-exists})
    (conflict! {:error "user_already_exists" :message "A user by that nickname already exists"}))
  (db-users/create-user xtdb-node nickname password)
  (log/info {:event :creating-user :nickname nickname})
  (let [{:keys [xt/id]} (db-users/get-user-by-name xtdb-node nickname)]
    (created (str "/users/" id) {:id id})))
