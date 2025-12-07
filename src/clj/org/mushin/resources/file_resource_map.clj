(ns org.mushin.resources.file-resource-map
  (:require [org.mushin.files :as files]
            [clojure.string :as cstr]
            [org.mushin.resources.resource-map :as interface]
            [lambdaisland.uri :refer [join]]
            [clojure.java.io :as io])
  (:import [java.nio.file Path]
           [java.io InputStream File]))

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
  ([^Path base resource-name]
   (files/path-combine base (get-resource-file-path resource-name)))
  ([resource-name]
   (let [name-length (count resource-name)]
     (files/path (if (> name-length 1) (subs resource-name 0 2) (subs resource-name 0 1))
                 (if (> name-length 3) (subs resource-name 2 4) "")
                 (if (> name-length 5) (subs resource-name 4 6) "")
                 resource-name))))


(defrecord FileSystemResourceMap
    [^Path base-path resource-map-url-base]
  interface/ResourceMap
  (create! [this name resource-data]
    (let [resource-path (get-resource-file-path base-path name)
          resource-path-str (str resource-path)]
      (when (files/not-exists resource-path)
        (io/make-parents resource-path-str)
        (cond
          (string? resource-data) (files/copy (files/path resource-data) resource-path)
          (instance? File resource-data) (files/copy (files/path (files/get-path resource-data)) resource-path)

          (instance? Path resource-data) (files/copy resource-data resource-path)

          ;; TODO this isn't atomic.
          (instance? InputStream resource-data) (with-open [output-file (io/output-stream resource-path-str)]
                                                  (io/copy resource-data output-file))))
      (interface/to-uri this name)))
  (delete! [_ name]
    (files/delete-if-exists (get-resource-file-path base-path name)))
  (exists? [_ name]
    (files/exists (get-resource-file-path base-path name)))
  (to-uri [_ name]
    (join resource-map-url-base (cstr/join "/" (files/path-parts (get-resource-file-path name)))))
  (open [_ name]
    (io/input-stream (str (get-resource-file-path base-path name)))))
