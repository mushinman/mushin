(ns org.mushin.db.status-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [org.mushin.test-support.db :as test-db]
            [org.mushin.db.users :as db-users]
            [org.mushin.db.util :as db]
            [org.mushin.db.statuses :as st]
            [org.mushin.db.relationship :as db-rel]
            [xtdb.api :as xt]
            [org.mushin.test-support.malli :as mfix]))

(use-fixtures :once mfix/malli-registry-fixture)

(def typical-user
  "A typical user."
  (db-users/create-local-user "ichijo" ""))

(def typical-status
  "A typical status"
  (st/create-status (:xt/id typical-user) "Total demon death" :text))

(def typical-status-2
  "Another typical status"
  (st/create-status (:xt/id typical-user) "mine is the way of the sword" :text))

(deftest inter-users-stauses-tx:inter-all-statuses
  (with-open [test-node (test-db/start-xtdb!)]
    (let [{:keys [node]} test-node]
      (xt/execute-tx node [[:put-docs :mushin.db/users typical-user]
                           [:put-docs :mushin.db/statuses typical-status typical-status-2]])
      ;; Delete all posts.
      (xt/execute-tx node [(st/inter-users-statuses-tx (:xt/id typical-user))])
      (is (zero?
           (count
            (xt/q
             node
             (xt/template (from :mushin.db/statuses [{:user ~(:xt/id typical-user)}])))))
          "All of user's statuses should be converted into tombstones."))))
