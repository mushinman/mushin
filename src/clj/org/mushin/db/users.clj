(ns org.mushin.db.users
  (:require [xtdb.api :as xt]
            [org.mushin.db.util :as db-util]
            [clj-uuid :as uuid]
            [buddy.hashers :as hashers]
            [org.mushin.db.authorization :as authz]
            [org.mushin.crypt.password :as crypt]
            [java-time.api :as jt]
            [org.mushin.db.statuses :as statuses]
            [malli.experimental.time :as mallt]
            [org.mushin.utils :refer [to-java-uri]]))

(def nickname-regex "Regular that describes a valid nickname" #"\w+")


(def nickname-schema
  "Schema for the nickname"
  [:nickname [:and [:and [:string {:min 1 :max 32}] [:re nickname-regex]]]])

(def ^:private user-states-schema
  "Schema for user states.
  | Key          | State                     | Meaning                                |
  |:-------------|:--------------------------|:---------------------------------------|
  | `:ok`        | None                      | Account activated and in good standing |
  | `:timeout`   | Time the timeout expires. | Account is in timeout                  |
  | `:tombstone` | None                      | Account is dead/deactivated.           |
  "
  [:multi {:dispatch :type}
   [:ok [:map [:type :keyword]]]
   [:timeout [:map [:type :keyword] [:timeout (mallt/-zoned-date-time-schema)]]]  ; TODO implement timeout
   [:tombstone [:map [:type :keyword]]]])

(def user-schema
  "Schema for users.
  | Key                 | Type      | Meaning                                                           |
  |:--------------------|:----------|:------------------------------------------------------------------|
  | `xt/id`             | UUID      | Row key                                                           |
  | `email`             | string    | User email address                                                |
  | `log-counter`       | int       | How many times this user has logged in counted at most once daily |
  | `nickname`          | string    | The user's nickname                                               |
  | `password-hash`     | string    | Password hash                                                     |
  | `local`             | bool      | True if the user is local, false if foreign                       |
  | `bio`               | string    | The user's biography                                              |
  | `joined-at`         | Timestamp | The time the user created their account                           |
  | `last-logged-in-at` | Timestamp | The last the user logged in                                       |
  | `privacy-level`     | keyword   | Level of privacy. Can be `:open`, `:open-instance`, `:locked`     |
  "
  {::tiny-string  [:string {:min 1 :max 32}]
   ::short-string [:string {:min 1 :max 256}]
   ::long-string  [:string {:min 1 :max 4096}]

   :mushin.db/users
   [:map
    [:xt/id                   :uuid]
    [:email {:optional true}  [:and ::short-string [:re #".+@.+"]]]
    [:log-counter             :int]
    [:nickname                :string]
    [:display-name            :string]
    [:avatar {:optional true} 'uri?]
    [:banner {:optional true} 'uri?]
    [:ap-id {:optional true}  'uri?]
    [:password-hash           :string]
    [:bio                     :string]
    [:state                   user-states-schema]
    [:privacy-level           [:enum :open :open-instance :locked]]
    [:local?                  :boolean]
    [:joined-at               (mallt/-zoned-date-time-schema)]
    [:last-logged-in-at       (mallt/-zoned-date-time-schema)] ; TODO maybe move to a new table for user events?
    authz/authorization-object-schema]})

(def safe-user-columns
  "A list of columns that are safe to return in the API (e.g. not the password)."
  '[xt/id log-counter nickname display-name avatar banner ap-id bio state privacy-level local? joined-at last-logged-in-at])

(defn is-valid-nickname
  "Checks if a nickname is valid.

  # Arguments
    - `nickname`: A nickname.

  # Return value
  True if the nickname is valid, false if not."
  [nickname]
  (re-matches nickname-regex nickname))

(defn get-user-by-id
  ([xtdb-node id] (get-user-by-id xtdb-node id safe-user-columns))
  ([xtdb-node id cols] (db-util/lookup-first xtdb-node :mushin.db/users cols {:xt/id id})))

(defn get-users-by-ids
  [db-con ids]
  (db-util/lookup-by-ids db-con :mushin.db/users ids))

(defn check-user-id-exists? [xtdb-node user-id]
  (db-util/record-exists? xtdb-node :mushin.db/users user-id))

(defn check-user-nickname-exists? [xtdb-node nickname]
  (db-util/lookup-exists-any? xtdb-node :mushin.db/users {:nickname nickname}))

(defn get-user-by-name
  ([xtdb-node nickname] (get-user-by-name xtdb-node safe-user-columns nickname))
  ([xtdb-node cols nickname] (db-util/lookup-first xtdb-node :mushin.db/users cols {:nickname nickname})))

(defn get-user-id-by-nickname
  [xtdb-node nickname]
  (-> (xt/q xtdb-node (xt/template (-> (from :mushin.db/users [xt/id {:nickname ~nickname}])
                                       (limit 1))))
      first
      :xt/id))

(defn create-local-user [nickname password ap-id avatar-uri banner-uri bio display-name & email]
  (let [now (jt/zoned-date-time)]
    (cond-> (merge
             {:xt/id (uuid/v7)
              :nickname nickname
              :ap-id (to-java-uri ap-id)
              :display-name display-name
              :local? true
              :state {:type :ok}
              :log-counter 0
              :avatar (to-java-uri avatar-uri)
              :banner (to-java-uri banner-uri)
              :bio bio
              :password-hash (crypt/hash-password password)
              :joined-at now
              :privacy-level :open
              :last-logged-in-at now}
             authz/default-object-doc)
      email (assoc :email email))))

(defn insert-user-tx
  [doc]
  (db-util/insert-unless-exists-tx
   :mushin.db/users
   doc
   [:nickname]))

(defn delete-user-tx
  "Create a transaction for deleting a user from the database.

  # Arguments
   - `user-id`: ID of the user to delete.

  # Return value
  Returns a transaction to remove the user from the following tables:
  `:mushin.db/actor-roles`,  `:mushin.db/relationships`, and erects a
  tombstone for `:mushin.db/users` and `:mushin.db/statuses`."
  [user-id]
  [[:patch-docs :mushin.db/users {:xt/id user-id
                                  :password-hash ""
                                  :state {:type :tombstone}}]
   (statuses/inter-users-statuses-tx user-id)
   (db-util/delete-where
    :mushin.db/actor-roles
    (xt/template (from :mushin.db/actor-roles [{:actor-id ~user-id}])))
   (db-util/delete-where
    :mushin.db/relationships
    (xt/template (-> (from :mushin.db/relationships [source target])
                     (where (or (= source ~user-id) (= target ~user-id)))
                     (limit 1))))])

(defn check-user-can-view-user
  "Check if the viewer can view the viewee.

  # Arguments
   - `xtdb-node`: XTDB node
   - `viewer-id`: ID of the user attempting to view the viewee.
   - `viewee-id`: ID of the user who is being viewed by the viewer.

  # Return value
  | Keyword    | Meaning                                                                          |
  |:-----------|:---------------------------------------------------------------------------------|
  | `:blocked` | The viewee has the viewer blocked.                                               |
  | `:locked`  | The viewee has a locked account, and the viewer does not permissions to view it. |
  | `:allowed` | The viewee can view the viewee.                                                  |
  "
  [xtdb-node viewer-id viewee-id]
  (->
   (xt/q
    xtdb-node
    (xt/template
     (->
      (unify
       (from :mushin.db/users [{:xt/id ~viewer-id} {:local? viewer-local?}])
       (from :mushin.db/users [{:xt/id ~viewee-id} {:privacy-level viewee-privacy-level}]))
      (with
       {:blocked? (exists? (from :mushin.db/relationships [{:type :block :source ~viewee-id :target ~viewer-id}]))
        :followed? (exists? (from :mushin.db/relationships [{:type :follow :source ~viewer-id :target ~viewee-id}]))})
      ;; As of XTDB 2.0.0 we need this second (with) expression because if-some and let don't work
      ;; (see https://github.com/xtdb/xtdb/issues/3179), and you can't use symbols defined in a (with)
      ;; inside the same expression.
      ;; TODO change to a if-some when the above bug is fixed.
      (with
       {:reject-reason
        (cond
          blocked? :blocked
          (or (and (= viewee-privacy-level :locked) (not followed?))
              (and (= viewee-privacy-level :open-instance) (not viewer-local?)))
          :locked
          ;; The "true" isn't required but it shuts up lsp.
          true false)})
      (return {:result
               (if reject-reason
                 reject-reason
                 :allowed)}))))
   first
   :result))


(defn can-login?
  "Check that a given password matches a given nickname.

  # Arguments
   - `xtdb-node`: Database.
   - `nickname`: User nickname.
   - `password`: The password for the `nickname`'s user account.

  # Return value
  The user's `:user-id` if a login is allowed,
  or a reason code for login rejection. Could be: `no-account`, `:timeout`,
  `:dead-account`, or `:wrong-nickname-or-password`"
  [xtdb-node nickname password]
  (let [{{:keys [type] :as state} :state :keys [password-hash xt/id]}
        (first (xt/q xtdb-node (xt/template (-> (from :mushin.db/users [{:nickname ~nickname} state password-hash xt/id])
                                                (limit 1)))))]
    (cond
      (not= type :ok)
      (case type
        nil :no-account
        :timeout :timeout
        :tombstone :dead-account
        (throw (ex-info "Invalid state for account" {:invalid-state :user :state state})))

      (and password-hash (:valid (hashers/verify password password-hash)))
      id

      :else
      :wrong-nickname-or-password)))

(defn check-nickname-and-password
  "Check that a given password matches a given nickname.
  Uses the same username/password verification as `can-login?`.

  # Arguments
   - `xtdb-node`: Database.
   - `nickname`: User nickname.
   - `password`: The password for the `nickname`'s user account.

  # Return value
  True if the user can login, false if not."
  [xtdb-node nickname password]
  (uuid? (can-login? xtdb-node nickname password)))
