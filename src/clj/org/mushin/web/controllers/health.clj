(ns org.mushin.web.controllers.health
  (:require
    [ring.util.http-response :refer :all]
    [clojure.tools.logging :as log]
    [org.mushin.db.util :as db-utils]
    [xtdb.api :as xt])
  (:import
    [java.util Date]
    [java.time Instant]
    [java.util UUID]))

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
  (try
    (ok (db-utils/submit-tx xtdb-node [[:mushin/upsert :user
                                        {:xt/id (UUID/randomUUID)
                                         :email "baby@gravy.com"
                                         :joined-at (Instant/now)
                                         :last-logged-in-at (Instant/now)}
                                        ]]))
    (catch Exception ex
      (log/error "Got an error: " ex)
      (internal-server-error "Some sort of error"))))
