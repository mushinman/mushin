(ns org.mushin.resources.file-resource-map
  (:require [org.mushin.files :as files]
            [clojure.string :as cstr]
            [org.mushin.resources.resource-map :as interface]
            [org.mushin.db.util :as db-util]
            [org.mushin.db.resource-meta :as res-meta]
            [lambdaisland.uri :refer [uri join]]
            [clojure.java.io :as io]
            [xtdb.api :as xt])
  (:import [java.nio.file Path]
           [java.io InputStream File]))

(defn- get-resource-path-as-vec
  [resource-name]
  (let [name-length (count resource-name)]
    [(if (> name-length 1) (subs resource-name 0 2) (subs resource-name 0 1))
     (if (> name-length 3) (subs resource-name 2 4) "")
     (if (> name-length 5) (subs resource-name 4 6) "")
     resource-name]))

(defn- get-resource-file-path
  "For a given base file path to the resource folder and the name of the resource
  generate a unique file path for the resource.
  # Arguments:
  - `base`: (Optional) A java Path to the resources folder.
  - `resource-name`: The name of the resource.

  # Return value
  A unique java Path for the provided resource name. If `base` is not provided
  the path is only partial."
  ^Path
  [^Path base resource-name]
  (apply files/path-combine base (get-resource-path-as-vec resource-name)))


(defrecord FileSystemResourceMap
    [^Path base-path resource-map-url-base xtdb-node]
  interface/ResourceMap
  (create! [this name resource-data]
    (let [resource-path (get-resource-file-path base-path name)
          resource-path-str (str resource-path)]
      (if-let [doc (interface/metadata this name)]
        doc
        (do
          (io/make-parents resource-path-str)
          (cond
            (string? resource-data)
            (files/copy (files/path resource-data) resource-path)

            (instance? File resource-data)
            (files/copy (files/path (files/get-path resource-data)) resource-path)

            (instance? Path resource-data)
            (files/copy resource-data resource-path)

            ;; TODO this isn't atomic.
            (instance? InputStream resource-data)
            (with-open [output-file (io/output-stream resource-path-str)]
              (io/copy resource-data output-file)))

          (let [doc (res-meta/create-resource-meta-doc
                     name
                     (join resource-map-url-base (cstr/join "/" (get-resource-path-as-vec name))))]
            (db-util/compose-and-execute-txs! xtdb-node (res-meta/insert-resource-tx doc))
            doc)))))
  (delete! [_ name]
    (db-util/compose-and-execute-txs! xtdb-node (res-meta/delete-resource-meta-tx name))
    (files/delete-if-exists (get-resource-file-path base-path name)))
  (metadata [_ name]
    (res-meta/get-resource-by-id xtdb-node name))
  (exists? [this name]
    (nil? (interface/metadata this name)))
  (to-uri [_ name]
    (when-let [location (:location (res-meta/get-resource-by-id xtdb-node name))]
      (uri location)))
  (open [_ name]
    (io/input-stream (str (get-resource-file-path base-path name)))))
