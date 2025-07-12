(ns org.mushin.web.controllers.health
  (:require
   [ring.util.http-response :refer :all]
   [clojure.tools.logging :as log]
   [org.mushin.db.util :as db-utils]
   [xtdb.api :as xt])
  (:import
   [java.util Date]
   [java.time Instant]))

(defn healthcheck!
  [_]
  (ok
   {:time     (str (Date. (System/currentTimeMillis)))
    :up-since (str (Date. (.getStartTime (java.lang.management.ManagementFactory/getRuntimeMXBean))))
    :app      {:status  "up"
               :message ""}}))

(defn db-status [{:keys [xtdb-node] :as opts}
                 {:keys [params] :as req}]
  (println " opts is " (pr-str opts))
  (try
    (db-utils/submit-tx xtdb-node [[:put-docs :user
                                    {:xt/id (random-uuid)
                                     :email "baby@gravy.com"
                                     :joined-at (Instant/now)
                                     :last-logged-in-at (Instant/now)}]])
    (ok "Success!!!!!")
    (catch Exception ex
      (log/error "Got an error: " ex)
      (internal-server-error "Some sort of error"))))

(defn dq-texas
  [{:keys [xtdb-node] :as opts}
   {:keys [params] :as req}]
  (println "@@@@@@@")
  (try
    (ok (xt/q xtdb-node '(-> (from :user [{:xt/id user-id, :email email-address}])
                             (where (= email-address "baby@gravy.com")))))
    (catch Exception ex
      (log/error "Got exeption " ex)
      (internal-server-error "Uhh?"))))
