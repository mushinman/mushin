(ns org.mushin.resources.file-resource-map
  (:require [org.mushin.files :as files]
            [org.mushin.resources.resource-map :as interface]
            [clojure.java.io :as io])
  (:import [java.nio.file Path]))

(defn- get-resource-file-path [base file-name]
  (let [name-length (count file-name)]
    (files/path-combine base
                        (if (> name-length 1) (subs file-name 0 2) (subs file-name 0 1))
                        (if (> name-length 3) (subs file-name 2 4) "")
                        (if (> name-length 5) (subs file-name 4 6) "")
                        file-name)))


(defrecord FileSystemResourceMap
    [^Path base-path]
  interface/ResourceMap
  (create! [this name input-stream]
    (let [resource-path (get-resource-file-path (:base-path this) name)
          resource-path-str (str resource-path)]
      (io/make-parents resource-path-str)
      (when (files/not-exists resource-path)
        (with-open [output-file (io/output-stream resource-path-str)]
          (io/copy input-stream output-file)))
      resource-path))
  (delete! [this name]
    (files/delete-if-exists (get-resource-file-path (:base-path this) name)))
  (exists? [this name]
    (files/exists (get-resource-file-path (:base-path this) name)))
  (open [this name]
    (io/input-stream (str (get-resource-file-path (:base-path this) name)))))
