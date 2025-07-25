(ns org.mushin.core
  (:require
   [clojure.tools.logging :as log]
   [integrant.core :as ig]
   [org.mushin.config :as config]
   [org.mushin.env :refer [defaults]]

    ;; Edges
   [kit.edge.cache.redis] 
   [kit.edge.http.hato] 
   [kit.edge.scheduling.quartz] 
   [kit.edge.utils.metrics] 
   [kit.edge.server.undertow]
   [org.mushin.web.handler]
   [org.mushin.xtdb]

    ;; Routes
   [org.mushin.web.routes.api])
  (:gen-class))

;; log uncaught exceptions in threads
(Thread/setDefaultUncaughtExceptionHandler
 (fn [thread ex]
   (log/error {:what :uncaught-exception
               :exception ex
               :where (str "Uncaught exception on" (.getName thread))})))

(defonce system (atom nil))

(defn stop-app []
  ((or (:stop defaults) (fn [])))
  (some-> (deref system) (ig/halt!)))

(defn start-app [& [params]]
  ((or (:start params) (:start defaults) (fn [])))
  (->> (config/system-config (or (:opts params) (:opts defaults) {}))
       (ig/expand)
       (ig/init)
       (reset! system)))

(defn -main [& _]
  (start-app)
  (.addShutdownHook (Runtime/getRuntime) (Thread. (fn [] (stop-app) (shutdown-agents)))))
