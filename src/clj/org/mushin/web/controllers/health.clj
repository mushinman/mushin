(ns org.mushin.web.controllers.health
  (:require
    [ring.util.http-response :refer :all]
    [clojure.tools.logging :as log]
    [xtdb.api :as xt])
  (:import
    [java.util Date]))

(defn healthcheck!
  [req]
  (ok
    {:time     (str (Date. (System/currentTimeMillis)))
     :up-since (str (Date. (.getStartTime (java.lang.management.ManagementFactory/getRuntimeMXBean))))
     :app      {:status  "up"
                :message ""}}))


(defn db-status [{:keys [xtdb-node] :as opts}
                 {:keys [params] :as req}]
  (println " opts is " (pr-str opts))
  (ok (xt/status xtdb-node)))
