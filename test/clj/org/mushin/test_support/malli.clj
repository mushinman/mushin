(ns org.mushin.test-support.malli
  (:require [clojure.test :refer [use-fixtures]]
            [org.mushin.config :as cfg]))

(defonce ^:private inited? (atom false))

(defn malli-registry-fixture [f]
  (when (compare-and-set! inited? false true)
    (cfg/init-db-malli!))
  (f))
