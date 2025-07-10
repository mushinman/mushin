(ns org.mushin.env
  (:require [clojure.tools.logging :as log]))

(def defaults
  {:init       (fn []
                 (log/info "\n-=[mushin starting]=-"))
   :start      (fn []
                 (log/info "\n-=[mushin started successfully]=-"))
   :stop       (fn []
                 (log/info "\n-=[mushin has shut down successfully]=-"))
   :middleware (fn [handler _] handler)
   :opts       {:profile :prod}})
