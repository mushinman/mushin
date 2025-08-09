(ns org.mushin.config
  (:require
   [kit.config :as config]
   [malli.registry :as mallr]
   [malli.core :as mallc]
   [malli.experimental.time :as malt]
   [org.mushin.db.users :as users]
   [org.mushin.db.resources :as resources]
   [org.mushin.db.remember-me :as remember-me]
   [org.mushin.db.statuses :as statuses]))

(defonce schema-store (atom {}))

(defn register-schema!
  "Register schema specification `spec` to key `k`."
  [k spec]
  (swap! schema-store assoc k spec))

(def ^:const system-filename "system.edn")

(defn init-db-malli!
  "Adds database schemas to the malli registry."
  []
  (let [all-schemas (merge users/user-schema statuses/statuses-schema resources/resources-schema remember-me/remember-me)]
    ;; Add our DB schemas.
    (mallr/set-default-registry!
     (mallr/composite-registry (mallc/default-schemas) (malt/schemas) all-schemas (mallr/mutable-registry schema-store)))))

(defn system-config
  [options]
  (init-db-malli!)
  (config/read-config system-filename options))
