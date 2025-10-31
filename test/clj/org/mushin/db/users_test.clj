(ns org.mushin.db.users-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [org.mushin.test-support.db :as test-db]
            [org.mushin.db.users :as db-users]
            [org.mushin.db.util :as db]
            [org.mushin.db.relationship :as db-rel]
            [xtdb.api :as xt]
            [org.mushin.test-support.malli :as mfix])
  (:import [xtdb.error Conflict]))

(use-fixtures :once mfix/malli-registry-fixture)

(deftest create-and-fetch-user-test
  (with-open [test-node (test-db/start-xtdb!)]
    (let [{:keys [node]} test-node]
      (db/execute-tx node [[:put-docs :mushin.db/users
                            (db-users/create-user "hikaru" "")]])
      (is (uuid? (db-users/get-user-id-by-nickname node "hikaru"))
          "User should exist"))))


(deftest can-user-view?-test
  (with-open [test-node (test-db/start-xtdb!)]
    (let [{:keys [node]} test-node
          user1 (db-users/create-user "ichijo" "")
          user2 (db-users/create-user "doshin" "")
          user1-id (:xt/id user1)
          user2-id (:xt/id user2)]
      (xt/execute-tx node (into [[:put-docs :mushin.db/users user1]
                                 [:put-docs :mushin.db/users user2]]
                                (db-rel/insert-relation-tx (db-rel/relationship-doc user2-id user1-id :block))))
      (is (= (db-users/check-user-can-view-user node user1-id user2-id) :blocked)
          "Viewer should not be able to view the viewee"))))

(deftest user-nickname-duplicate-fails?
  (with-open [test-node (test-db/start-xtdb!)]
    (let [{:keys [node]} test-node
          user1 (db-users/create-user "ichijo" "")]
      (xt/execute-tx node (db-users/insert-user-tx user1))
      (is (thrown? Conflict (xt/execute-tx node (db-users/insert-user-tx user1)))
          "Duplicate usernames should result in an exception."))))


(deftest block-overrides
  (with-open [test-node (test-db/start-xtdb!)]
    (let [{:keys [node]} test-node
          user1 (db-users/create-user "ichijo" "")
          user2 (db-users/create-user "doshin" "")
          user1-id (:xt/id user1)
          user2-id (:xt/id user2)]
      (xt/execute-tx node (into [[:put-docs :mushin.db/users user1]
                                 [:put-docs :mushin.db/users user2]]
                                (db-rel/insert-relation-tx (db-rel/relationship-doc user2-id user1-id :block))))
      (xt/execute-tx node (db-rel/insert-relation-tx (db-rel/relationship-doc user2-id user1-id :block)))
      (is (= (count (xt/q node (xt/template (-> (from :mushin.db/relationships [{:source ~user2-id
                                                                                 :target ~user1-id
                                                                                 :type :block}])
                                                (limit 1)))))
             1)
          "A block between two users should override a previous block."))))
