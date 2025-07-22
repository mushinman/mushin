(ns org.mushin.db.users
  (:require [xtdb.api :as xt]
            [org.mushin.db.util :as db-util]
            [org.mushin.crypt.password :as crypt]
            [java-time.api :as jt]
            [malli.experimental.time :as mallt]))

(def nickname-schema
  [:nickname  [:and [:string {:min 1 :max 32}] [:re #"\w+"]]])

(def user-schema
  {::tiny-string  [:string {:min 1 :max 32}]
   ::short-string [:string {:min 1 :max 256}]
   ::long-string  [:string {:min 1 :max 4096}]

   :mushin.db/users [:map
                     [:xt/id     :uuid]
                     [:email  {:optional true}  [:and ::short-string [:re #".+@.+"]]]
                     nickname-schema
                     [:password-hash :string]
                     [:joined-at (mallt/-zoned-date-time-schema)]
                     [:last-logged-in-at (mallt/-zoned-date-time-schema)]]})


(defn check-user-exists? [node user-id]
  (db-util/record-exists? node :mushin.db/users user-id))

(defn get-user-by-name [node nickname]
  (first (xt/q node (xt/template
                     (-> (from :mushin.db/users [* {:nickname ~nickname}])
                         (limit 1))))))

(defn check-user-exists-by-name? [node nickname]
  (boolean (first (xt/q node (xt/template
                              (-> (from :mushin.db/users [{:nickname ~nickname}])
                                  (limit 1)))))))


(defn create-user [xtdb-node nickname password & email]
  (let [now (jt/zoned-date-time)
        doc {:xt/id (random-uuid)
             :nickname nickname
             :password-hash (crypt/hash-password password)
             :joined-at now
             :last-logged-in-at now}]
    (db-util/execute-tx xtdb-node
                        [[:put-docs :mushin.db/users
                          (if email
                            (assoc doc :email email)
                            doc)]])))
