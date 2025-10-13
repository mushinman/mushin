(ns org.mushin.web.controllers.accounts
  (:require
            [org.mushin.db.users :as db-users]
            [ring.util.http-response :refer [conflict! created]]
            [clojure.tools.logging :as log]
            [org.mushin.db.util :as db]))

(def create-account-body
  [:map
   [:email {:optional true} [:and :string [:re #".+@.+"]]]
   [:password [:string {:min 8 :max 128}]]
   db-users/nickname-schema
   ; TODO recapcha
   ])

(defn create-account-post!
  [{:keys [xtdb-node]}
   {{{:keys [nickname password]} :body} :parameters}]
  (when (db-users/check-user-nickname-exists? xtdb-node nickname)
    (log/info {:event :creating-user-failed :nickname nickname :reason :user-already-exists})
    (conflict! {:error "user_already_exists" :message "A user by that nickname already exists"}))
  (log/info {:event :creating-user :nickname nickname})
  (let [{:keys [xt/id]} (db/submit-tx xtdb-node
                                      [[:put-docs :mushin.db/users
                                        (db-users/create-user xtdb-node nickname password)]]) ]
    (created (str "/users/" id) {:id id})))
