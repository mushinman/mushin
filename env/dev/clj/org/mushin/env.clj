(ns org.mushin.env
  (:require
    [clojure.tools.logging :as log]
    [org.mushin.dev-middleware :refer [wrap-dev]]))

(def defaults
  {:init       (fn []
                 (log/info "\n-=[mushin starting using the development or test profile]=-"))
   :start      (fn []
                 (log/info "\n-=[mushin started successfully using the development or test profile]=-"))
   :stop       (fn []
                 (log/info "\n-=[mushin has shut down successfully]=-"))
   :middleware wrap-dev
   :opts       {:profile       :dev}})
