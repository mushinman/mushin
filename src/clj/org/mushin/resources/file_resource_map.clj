(ns org.mushin.resources.file-resource-map
  (:require [org.mushin.files :as files]
            [org.mushin.resources.resource-map :as interface]
            [clojure.java.io :as io])
  (:import [java.nio.file Path]
           [java.io InputStream File]))

(defn- get-resource-file-path [base file-name]
  (let [name-length (count file-name)]
    (files/path-combine base
                        (if (> name-length 1) (subs file-name 0 2) (subs file-name 0 1))
                        (if (> name-length 3) (subs file-name 2 4) "")
                        (if (> name-length 5) (subs file-name 4 6) "")
                        file-name)))

; (get-resource-file-path (files/path "content") "vyk1C6u3pcdfTIlvEFSdjvHBiymJBxUuvcZj3BR5Ol.png")

(defrecord FileSystemResourceMap
    [^Path base-path]
  interface/ResourceMap
  (create! [this name resource-data]
    (let [resource-path (get-resource-file-path (:base-path this) name)
          resource-path-str (str resource-path)]
      (when (files/not-exists resource-path)
        (io/make-parents resource-path-str)
        (cond
          (string? resource-data) (files/copy (files/path resource-data) resource-path)
          (instance? File resource-data) (files/copy (files/path (files/get-path resource-data)) resource-path)

          (instance? Path resource-data) (files/copy resource-data resource-path)

          (instance? InputStream resource-data) (with-open [output-file (io/output-stream resource-path-str)]
                                                  (io/copy resource-data output-file))))
      name))
  (delete! [this name]
    (files/delete-if-exists (get-resource-file-path (:base-path this) name)))
  (exists? [this name]
    (files/exists (get-resource-file-path (:base-path this) name)))
  (open [this name]
    (io/input-stream (str (get-resource-file-path (:base-path this) name)))))
