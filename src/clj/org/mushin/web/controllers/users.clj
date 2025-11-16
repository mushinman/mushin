(ns org.mushin.web.controllers.users
  (:require [org.mushin.db.users :as db-users]
            [ring.util.http-response :refer [conflict! created ok]]
            [org.mushin.resources.resource-map :as res]
            [clojure.tools.logging :as log]
            [org.mushin.db.util :as db]))

(def create-user-body
  [:map
   [:email {:optional true} [:and :string [:re #".+@.+"]]]
   [:password [:string {:min 8 :max 128}]]
   [:avatar  {:description "mulitpart file" :optional true} :any]
   [:banner  {:description "mulitpart file" :optional true} :any]
   db-users/nickname-schema
   ; TODO recapcha
   ])

(defn create-user!
  [{:keys [xtdb-node resource-map]}
   {{{:keys [nickname password avatar banner bio display-name]
      :or {bio ""
           display-name ""}} :body} :parameters :keys [mushin/async?]}]
  (when (db-users/check-user-nickname-exists? xtdb-node nickname)
    (log/info {:event :creating-user-failed :nickname nickname :reason :user-already-exists})
    (conflict! {:error :user-already-exists :message "A user by that nickname already exists"}))
  (log/info {:event :creating-user :nickname nickname})
  (let [{:keys [xt/id] :as doc} (db-users/create-user nickname password
                                                      (res/to-url resource-map "default-avatar.png")
                                                      (res/to-url resource-map "default-banner.png")
                                                      bio display-name)]
    (if async?
      (do
        (db/submit-tx xtdb-node
                      [[:put-docs :mushin.db/users doc]])
        (created (str "/users/" id) {:id id}))
      (do
        (db/execute-tx xtdb-node
                       [[:put-docs :mushin.db/users doc]])
        (ok {:id id})))))
