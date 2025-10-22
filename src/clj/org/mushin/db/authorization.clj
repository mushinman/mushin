(ns org.mushin.db.authorization
  (:require [clj-uuid :as uuid]
            [org.mushin.db.util :as db]
            [org.mushin.utils :refer [contains-one-of?]]
            [org.mushin.utils :as util]
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

(def authorization-subject-schema
  [:map
   [:roles         [:set :uuid]]
   [:tags          [:set :keyword]]
   [:capabilities  [:set :keyword]]])

(def authorization-object-schema
  [:map
   [:tags          [:set :keyword]]])

(def authorization-role-schema
  "Schema for role.
  | Key            | Type         | Meaning                                            |
  |:---------------|:-------------|:---------------------------------------------------|
  | `id`           | uuid         | Row ID.                                            |
  | `name`         | string       | The name of the role.                              |
  | `rules`        | map          | Vector of rules.                                   |
  | `capabilities` | set[keyword] | Set of admin capabilities like :admin.roles:write. |
  "
  {:mushin.db/authz
   [:map {:closed true}
    [:xt/id            :uuid]
    [:name             :string]
    [:rules            [:vec authorization-effect-schema]]
    [:capabilities     [:set :keyword]]]})

(def default-subject-doc
  "An empty subject document, conforming to `authorization-subject-schema`."
  {:roles #{}
   :tags #{}
   :capabilities #{}})

(def default-object-doc
  "An empty object document, conforming to `authorization-object-schema`."
  {:tags #{}})

(defn create-role-doc
  "Create a role document. Will allocate a new `xt/id`.

  # Arguments
    - `name`: The name of the role.
    - `rules`: The rules for the role.

  # Return value
  A new role document."
  [name rules]
  {:xt/id (uuid/v7)
   :name name
   :rules rules
   :capabilities #{}})

(defn get-role-by-name
  "Get role by name.

  # Arguments
    - `xtdb-node`: A DB instance.
    - `name`: The name to serach for.

  # Return value
  The role with the name `name`, or nil if no such role exists."
  [xtdb-node name]
  (xt/q xtdb-node (xt/template (-> (from :mushin.db/authz [* {:name ~name}])
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

(defn can-<verb>-user?
  [{{:keys [capabilities]} :subject :keys [xt/id]} verb object-user-id]
  (or (= id object-user-id)
      (case verb
        (:create :update)
        (contains-one-of? capabilities :user.profile:create :user.profile:update)
        :delete
        (contains? capabilities :user.profile:delete)
        :view
        (contains? capabilities :user.profile:view))))

(defn allowed?
  [subject verb object-type object]
  (case object-type
    ;; User can only modify if self, or has the capability.
    :user (can-<verb>-user? subject verb (:xt/id object))
    ;; Ditto for statuses.
    :status (can-<verb>-user? subject verb (:user object))))

(defn can-user?
  "Determine if a user can perform an action on an object.

  # Arguments
    - `xtdb-node`: A DB instance.
    - `user`: The 'subject' of the action, a user acctount. Either a UUID or a nickname.
    - `action`: The action to perform on `object`.
    - `object`: The object to perform the action on.

  # Return value
  `true` if `user` can perform `action` on `object`, otherwise `false`."
  [xtd-node user action object]
  )
