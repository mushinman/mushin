(ns org.mushin.db.users-test
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [org.mushin.test-support.db :as test-db]
            [org.mushin.db.users :as db-users]
            [org.mushin.test-support.malli :as mfix]))

(use-fixtures :once mfix/malli-registry-fixture)

(deftest create-and-fetch-user-test
  (with-open [test-node (test-db/start-xtdb!)]
    (let [{:keys [node]} test-node]
      (db-users/create-user node "ogrubes" "idk")
      (is (uuid? (db-users/get-user-id-by-nickname node "ogrubes")) "User should exist"))))
