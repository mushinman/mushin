(ns org.mushin.db.tasks
  (:require [integrant.core :as ig]
            [buddy.core.keys :as sign-keys]
            [clojure.tools.logging :as log]
            [kit.ig-utils :as ig-utils]
            [org.mushin.db.util :as db]
            [xtdb.api :as xt]
            [org.mushin.db.remember-me :as db-tokens]))

(defmethod ig/init-key :org.mushin.db.tasks/tasks [_ {:keys [xtdb-node]}]
  (log/info "Initializing db tasks...")
  (reify org.quartz.Job
    (execute [_this _]
      (log/info "Starting db tasks...")
      (log/info "Purging the db of old keys.")
      (db/submit-tx xtdb-node [db-tokens/purge-invalid-tokens-query])
      (log/info "End db taskss."))))

(defmethod ig/suspend-key! :org.mushin.db.tasks/tasks [_ _]
  (log/info "Suspending db tasks"))

(defmethod ig/resume-key :org.mushin.db.tasks/tasks
  [key opts old-opts old-impl]
  (log/info "Resuming suspended db tasks")
  (ig-utils/resume-handler key opts old-opts old-impl))
