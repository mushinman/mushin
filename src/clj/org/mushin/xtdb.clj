(ns org.mushin.xtdb
  (:require
    [integrant.core :as ig]
    [clojure.tools.logging :as log]
    [xtdb.api       :as xt]
    [xtdb.node      :as node]))

;; Integrant init for key :db.xtdb/node
(defmethod ig/init-key :db.xtdb/node
  [_   ;; the key
   cfg] ;; the map you put in system.edn
  ;; node/->config will fill in defaults, compactor, etc.
  (node/start-node (node/->config cfg)))

;; Integrant halt for graceful shutdown
(defmethod ig/halt-key! :db.xtdb/node
  [_  ;; the key
   node]
  (.close node))
