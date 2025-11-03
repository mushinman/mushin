(ns org.mushin.resources
  (:require [integrant.core :as ig]
            [clojure.tools.logging :as log]
            [org.mushin.resources.file-resource-map :as fsm]
            [org.mushin.files :as files]
            [lambdaisland.uri :refer [uri]]
            [kit.ig-utils :as ig-utils]))

(defmethod ig/init-key :org.mushin.resources/provider [_ location]
  (log/info "Initializing the resource store provider...")
  (cond
    (contains? location :local)
    (let [{:keys [path base-url] :as config} (:local location)]
      (log/info "Creating filesystem resource provider with config:" config)
      (fsm/->FileSystemResourceMap (files/path path) (uri base-url)))

    :else (throw (ex-info "Unrecognized resource store provider settings" {:settings location}))))

(defmethod ig/suspend-key! :org.mushin.resources/provider  [_ _]
  (log/info "Suspending resource provider"))

(defmethod ig/resume-key :org.mushin.resources/provider
  [key opts old-opts old-impl]
  (log/info "Resuming suspended resource provider")
  (ig-utils/resume-handler key opts old-opts old-impl))
