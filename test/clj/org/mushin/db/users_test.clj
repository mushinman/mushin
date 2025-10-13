(ns org.mushin.db.users-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [org.mushin.test-support.db :as test-db]
            [org.mushin.db.users :as db-users]
            [org.mushin.db.util :as db]
            [org.mushin.test-support.malli :as mfix]))

(use-fixtures :once mfix/malli-registry-fixture)

(deftest create-and-fetch-user-test
  (with-open [test-node (test-db/start-xtdb!)]
    (let [{:keys [node]} test-node]
      (db/execute-tx node [[:put-docs :mushin.db/users
                            (db-users/create-user "ogrubes" "idk")]])
      (is (uuid? (db-users/get-user-id-by-nickname node "ogrubes")) "User should exist"))))
