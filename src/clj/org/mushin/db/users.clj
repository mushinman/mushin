(ns org.mushin.db.users
  (:require [xtdb.api :as xt]
            [org.mushin.db.util :as db-util]
            [org.mushin.crypt.password :as crypt]
            [java-time.api :as jt]
            [malli.experimental.time :as mallt]))

(def nickname-schema
  [:nickname  [:and [:and [:string {:min 1 :max 32}] [:re #"\w+"]]]])

(def user-schema
  "Schema for users.
  | Key                 | Type      | Meaning                                                           |
  |:--------------------|:----------|:------------------------------------------------------------------|
  | `xt/id`             | UUID      | Row key                                                           |
  | `email`             | string    | User email address                                                |
  | `log-counter`       | int       | How many times this user has logged in counted at most once daily |
  | `nickname`          | string    | The user's nickname                                               |
  | `password-hash`     | string    | Password hash                                                     |
  | `description`       | string    | The user's description                                            |
  | `joined-at`         | Timestamp | The time the user created their account                           |
  | `last-logged-in-at` | Timestamp | The last the user logged in                                       |
  "
  {::tiny-string  [:string {:min 1 :max 32}]
   ::short-string [:string {:min 1 :max 256}]
   ::long-string  [:string {:min 1 :max 4096}]

   :mushin.db/users [:map
                     [:xt/id     :uuid]
                     [:email  {:optional true}  [:and ::short-string [:re #".+@.+"]]]
                     [:log-counter     :int]
                     [:nickname        :string]
                     [:password-hash   :string]
                     [:description     :string]
                     [:joined-at (mallt/-zoned-date-time-schema)]
                     [:last-logged-in-at (mallt/-zoned-date-time-schema)]]})

(defn get-user-by-id
  ([xtdb-node id] (db-util/lookup-by-id xtdb-node :mushin.db/users id))
  ([xtdb-node cols id] (db-util/lookup-first xtdb-node :mushin.db/users cols {:xt/id id})))

(defn check-user-id-exists? [xtdb-node user-id]
  (db-util/record-exists? xtdb-node :mushin.db/users user-id))

(defn check-user-nickname-exists? [xtdb-node nickname]
  (db-util/lookup-exists-any? xtdb-node :mushin.db/users {:nickname nickname}))

(defn get-user-by-name
  ([xtdb-node nickname] (get-user-by-name xtdb-node '[*] nickname))
  ([xtdb-node cols nickname] (db-util/lookup-first xtdb-node :mushin.db/users cols {:nickname nickname})))

(defn get-user-id-by-nickname
  [xtdb-node nickname]
  (-> (xt/q xtdb-node (xt/template (-> (from :mushin.db/users [xt/id {:nickname ~nickname}])
                                       (limit 1))))
      first
      :xt/id))

(defn create-user [xtdb-node nickname password & email]
  (let [now (jt/zoned-date-time)
        doc {:xt/id (random-uuid)
             :nickname nickname
             :log-counter 0
             :description ""
             :password-hash (crypt/hash-password password)
             :joined-at now
             :last-logged-in-at now}]
    (db-util/execute-tx xtdb-node
                        [[:put-docs :mushin.db/users
                          (if email
                            (assoc doc :email email)
                            doc)]])
    doc))
