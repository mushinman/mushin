(ns org.mushin.db.db-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [org.mushin.test-support.db :as test-db]
            [org.mushin.db.users :as db-users]
            [org.mushin.db.util :as db]
            [xtdb.api :as xt]
            [org.mushin.test-support.malli :as mfix]))

(use-fixtures :once mfix/malli-registry-fixture)

(deftest delete-where:deletes-duplicate-rows
  (with-open [test-node (test-db/start-xtdb!)]
    (let [{:keys [node]} test-node
          user1 (db-users/create-local-user "ichijo" "")
          user1-id (:xt/id user1)]
      (xt/execute-tx node [[:put-docs :mushin.db/users user1]
                           [:put-docs :mushin.db/users user1]])
      (xt/execute-tx node [(db/delete-where :mushin.db/users {:xt/id user1-id})])
      (is (= (count (xt/q node (xt/template (from :mushin.db/users [{:xt/id ~user1-id}])))) 0)
          "All ichijos should be deleted"))))
