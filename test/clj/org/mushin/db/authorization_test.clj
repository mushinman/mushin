(ns org.mushin.db.authorization-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [org.mushin.test-support.db :as test-db]
            [clojure.set :as set]
            [xtdb.api :as xt]
            [org.mushin.db.users :as db-users]
            [org.mushin.db.authorization :as authz]))

(def block-18+
  {:action :view, :effect :deny :tags #{:18+}, :except-tags #{}, :match :any})

(def minor-role "For under 18 users."
  (authz/role-doc
   "minor"
   [block-18+]))

(def mod-role "For mods."
  (authz/role-doc
   "mod"
   []
   :user.profile:update :user.profile:delete :user.profile:view))

(def auditor-role "Can the audit log and not much else..."
  (authz/role-doc
   "auditor"
   []
   :admin.audit-log:view))

(def reporter-role "Can see every post and not much else..."
  (authz/role-doc
   "auditor"
   []
   :user.profile:view))

(def konatsu "A minor user."
  (db-users/create-local-user "konatsu" ""))

(deftest compress-many-role-perms
  (is (= (second (authz/compress-roles [mod-role auditor-role reporter-role]))
         (set/union (:perms mod-role) (:perms auditor-role) (:perms reporter-role)))))

(deftest compress-mod-perms
  (is (= (second (authz/compress-roles [mod-role])) (:perms mod-role))))

(deftest retrieved-one-role
  (with-open [test-node (test-db/start-xtdb!)]
    (let [{:keys [node]} test-node]
      (xt/execute-tx node (into
                           []
                           (concat
                            [[:put-docs :mushin.db/users konatsu]]
                            (authz/insert-role-tx minor-role)
                            (authz/insert-actor-role-tx (authz/actor-role-doc (:xt/id minor-role) (:xt/id konatsu))))))
      (is (= 1 (count (authz/actor-roles node (:xt/id konatsu))))))))
