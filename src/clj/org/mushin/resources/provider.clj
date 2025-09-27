(ns org.mushin.resources.provider
  (:require [integrant.core :as ig]
            [clojure.tools.logging :as log]
            [org.mushin.resources.file-resource-map :as fsm]
            [org.mushin.files :as files]
            [kit.ig-utils :as ig-utils]))


(defmethod ig/init-key :org.mushin.resources/provider [_ {:keys [location]}]
  (log/info "Initializing the resource store provider...")
  (cond
    (contains? location :path) (do
                                 (log/info "Create filesystem resource provider with location:" {:path (location :path) })
                                 (fsm/->FileSystemResourceMap (files/path (location :path))))
    :else (throw (ex-info "Unrecognized resource store provider settings" {:settings location}))))

(defmethod ig/suspend-key! :org.mushin.resources/provider  [_ _]
  (log/info "Suspending resource provider"))

(defmethod ig/resume-key :org.mushin.resources/provider
  [key opts old-opts old-impl]
  (log/info "Resuming suspended resource provider")
  (ig-utils/resume-handler key opts old-opts old-impl))
