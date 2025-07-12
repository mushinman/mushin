(ns org.mushin.config
  (:require
    [kit.config :as config]
    [malli.registry :as mr]
    [malli.core :as malli]
    [malli.experimental.time :as malt]
    [org.mushin.db.users :as users]))

(def ^:const system-filename "system.edn")

(defn init-db-malli!
  "Adds database schemas to the malli registry."
  []
  (let [all-schemas users/schema]
    (mr/set-default-registry!
     (mr/composite-registry (malli/default-schemas) (malt/schemas) all-schemas))))

(defn system-config
  [options]
  (init-db-malli!)
  (config/read-config system-filename options))
