(ns org.mushin.db.relationship-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [org.mushin.test-support.db :as test-db]
            [org.mushin.db.users :as db-users]
            [org.mushin.db.util :as db]
            [org.mushin.db.relationship :as db-rel]
            [xtdb.api :as xt]
            [org.mushin.test-support.malli :as mfix]))


(use-fixtures :once mfix/malli-registry-fixture)

