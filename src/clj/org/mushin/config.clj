(ns org.mushin.config
  (:require
   [kit.config :as config]
   [clojure.tools.logging :as log]
   [malli.registry :as mallr]
   [org.mushin.utils :refer [concat-kw]]
   [malli.core :as mallc]
   [malli.util :as mallu]
   [malli.experimental.time :as malt]
   [org.mushin.schema.db.users :as users]))

(defonce schema-store (atom {}))

(defn register-schema!
  "Register schema specification `spec` to key `k`."
  [k spec]
  (swap! schema-store assoc k spec))

(def ^:const system-filename "system.edn")

(defn init-db-malli!
  "Adds database schemas to the malli registry."
  []
  (let [all-schemas users/schema]
    ;; Add our DB schemas.
    (mallr/set-default-registry!
     (mallr/composite-registry (mallc/default-schemas) (malt/schemas) all-schemas (mallr/mutable-registry schema-store)))))

(defn system-config
  [options]
  (init-db-malli!)
  (config/read-config system-filename options))
