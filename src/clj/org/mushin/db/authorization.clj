(ns org.mushin.db.authorization
  (:require [clj-uuid :as uuid]
            [org.mushin.db.util :as db]
            [org.mushin.utils :refer [contains-one-of? contains-key?] :as util]
            [clojure.set :as set]
            [xtdb.api :as xt]))


(def authorization-effect-schema
  "Schema for an effect.
  | Key             | Type         | Meaning                                                                                                                               |
  |:----------------|:-------------|:--------------------------------------------------------------------------------------------------------------------------------------|
  | `action`        | keyword      | `view`, `create`, `edit`, `delete`, `extend`. `extend` is actions like replying.                                                      |
  | `effect`        | keyword      | `allow` or `deny`                                                                                                                     |
  | `tags`          | set[keyword] | A set of tags the effect can apply to.                                                                                                |
  | `except-tags`   | set[keyword] | A set of tags the effect does not apply to.                                                                                           |
  | `match`         | keyword      | `:any` or `:all`. If `:any` only 1 tag needs to match for the result to apply. If `:all` then all tags to match for the rule to apply |
  "
  [:map {:closed true}
   [:action              [:enum :view :create :edit :delete :extend]]
   [:effect              [:enum :allow :deny]]
   [:tags                [:set :keyword]]
   [:except-tags         [:set :keyword]]
   [:match               [:enum :any :all]]])

(def authorization-object-schema
  [:tags          [:set :keyword]])

(def authorization-role-schema
  "Schema for role.
  | Key            | Type         | Meaning                                            |
  |:---------------|:-------------|:---------------------------------------------------|
  | `id`           | uuid         | Row ID.                                            |
  | `name`         | string       | The name of the role.                              |
  | `rules`        | map          | Vector of rules.                                   |
  | `perms` | set[keyword] | Set of admin perms like :admin.roles:write. |

  Roles have the constraint that the `:name` column must be unique.
  "
  {:mushin.db/roles
   [:map {:closed true}
    [:xt/id            :uuid]
    [:name             :string]
    [:rules            [:vec authorization-effect-schema]]
    [:perms     [:set :keyword]]]})

(def default-object-doc
  "An empty object document, conforming to `authorization-object-schema`."
  {:tags #{}})

;; If XTDB ever supports tuples for xt/id this would be better represented as:
;; [:xt/id [:tuple :uuid :uuid]] where the UUIDs are a role-id and a user-id
;; respectively.
(def actor-role-schema
  {:mushin.db/actor-roles
   [:map
   [:xt/id          :uuid]
   [:role-id        :uuid]
   [:actor-id       :uuid]]})

(defn actor-role-doc
  [role-id actor-id]
  {:xt/id (uuid/v7)
   :role-id role-id
   :actor-id actor-id})

(defn insert-actor-role-tx
  [doc]
  (db/insert-unless-exists-tx :mushin.db/actor-roles doc :role-id :actor-id))

(defn insert-role-tx
  [doc]
  (db/insert-unless-exists-tx :mushin.db/roles doc :name))

(defn role-doc
  "Create a role document. Will allocate a new `xt/id`.

  # Arguments
    - `name`: The name of the role.
    - `rules`: The rules for the role.

  # Return value
  A new role document."
  [name rules & perms]
  {:xt/id (uuid/v7)
   :name name
   :rules rules
   :perms (set perms)})

(defn get-role-by-name
  "Get role by name.

  # Arguments
    - `xtdb-node`: A DB instance.
    - `name`: The name to serach for.

  # Return value
  The role with the name `name`, or nil if no such role exists."
  [xtdb-node name]
  (xt/q xtdb-node (xt/template (-> (from :mushin.db/roles [* {:name ~name}])
                                   (limit 1)))))


(defn do-rules-allow?
  [rules act object-tags]
  (let [object-tags (set object-tags)
        effects ; Relevant effects.
        (filter
         (fn [{:keys [action tags match except-tags]}]
           (and (= action act)
                ;; Exclude if in except tags.
                (util/disjoint? (set except-tags) object-tags)
                ;; Compare tags.
                (case match
                  :any (util/intersecting? (set tags) object-tags)
                  :all (set/subset? (set tags) object-tags)
                  ;; Ignore this rule if it doesn't have a match rule.
                  false)))
         rules)]
    (boolean
     (and (not-empty effects)
          (every?
           (fn [{:keys [effect]}]
             (= effect :allow))
           effects)))))

(defn object-tags
  [xtdb-node table object-id]
  (->
   (xt/q xtdb-node (xt/template
                   (from ~table [tags {:xt/id ~object-id}])))
   first
   :tags))

(defn permissions-allow?
  [perms action object-type]
  (case object-type
    (:user :status) (case action
                      :update
                      (contains-key? perms :user.profile:update)
                      :delete
                      (contains-key? perms :user.profile:delete)
                      :view
                      (contains-key? perms :user.profile:view)
                      false)))

(defn compress-roles
  "Compress roles into a vector of their effects and a set of their permissions.

  # Arguments
   - `roles`: A seq of role data.

  # Return value
  A tuple with a vector of every rule of every role, and a set of permissions."
  [roles]
  (reduce
   (fn [[effects all-perms] {:keys [rules perms]}]
     [(into effects (flatten rules)) (set/union all-perms perms)])
   [[] #{}]
   roles))

(defn actor-roles
  [xtdb-node user-id]
  (->
   (xt/q
    xtdb-node
    (xt/template
     (-> (unify (from :mushin.db/actor-roles [role-id {:actor-id ~user-id}])
                (from :mushin.db/roles [name rules perms {:xt/id role-id}]))
         (return name rules perms))))))
