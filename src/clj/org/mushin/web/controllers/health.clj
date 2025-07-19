(ns org.mushin.web.controllers.health
  (:require
   [ring.util.http-response :refer :all]
   [clojure.tools.logging :as log]
   [org.mushin.db.util :as db-utils]
   [java-time.api :as jt]
   [org.mushin.utils :refer :all]
   [buddy.hashers :as bhash]
   [xtdb.api :as xt])
  (:import
   [java.util Date]))

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
    (db-utils/submit-tx xtdb-node [[:put-docs :mushin.db/users
                                    {:xt/id (random-uuid)
                                     :email "baby@gravy.com"
                                     :password-hash (bhash/derive "@test" {:alg :argon2id})
                                     :nickname "babygravy"
                                     :joined-at (jt/zoned-date-time)
                                     :last-logged-in-at (jt/zoned-date-time)}]])
    (ok "Success!!!!!")
    (catch Exception ex
      (log/error "Got an error: " ex)
      (internal-server-error "Some sort of error"))))

(defn dq-texas
  [{:keys [xtdb-node] :as opts}
   {:keys [params] :as req}]
  (println "@@@@@@@")
  (try
    (ok (xt/q xtdb-node '(-> (from :mushin.db/users [{:xt/id user-id, :email email-address, :joined-at joined}])
                             (where (= email-address "baby@gravy.com")))))
    (catch Exception ex
      (log/error "Got exeption " ex)
      (internal-server-error "Uhh?"))))

(defn dbq-patch [{:keys [xtdb-node] :as opts}
                 {:keys [params] :as req}]
  (println " opts is " (pr-str opts))
  (try
    (db-utils/submit-tx xtdb-node [[:patch-docs :mushin.db/users
                                    {:xt/id (random-uuid)
                                     :email "baby@gravy.com"
                                     :joined-at (jt/zoned-date-time)
                                     :last-logged-in-at (jt/zoned-date-time)}]])
    (ok "Success!!!!!")
    (catch Exception ex
      (log/error "Got an error: " ex)
      (internal-server-error "Some sort of error"))))

(defn dbq-patch-incomplete [{:keys [xtdb-node] :as opts}
                 {:keys [params] :as req}]
  (println " opts is " (pr-str opts))
  (try
    (db-utils/submit-tx xtdb-node [[:patch-docs :mushin.db/users
                                    {:xt/id #uuid "f76c8413-5412-4fcb-9242-31e8675b2b52"
                                     :email "baby@gravy.com"
                                     :last-logged-in-at (jt/zoned-date-time)}]])
    (ok "Success!!!!!")
    (catch Exception ex
      (log/error "Got an error: " ex)
      (internal-server-error "Some sort of error"))))

(defn auth-test-post [{:keys [xtdb-node]} req]
  (log/info "It worked!!!")
  (ok {:message "Okay!!!"}))
