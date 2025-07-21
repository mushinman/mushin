(ns org.mushin.db.resources
  (:require [clj-commons.digest :as digest]
            [clojure.java.io :as io]
            [org.mushin.db.util :as db]
            [java-time.api :as jt]
            [clojure.string :as cstr]
            [xtdb.api :as xt])
  (:import [java.nio.file Files StandardCopyOption Paths]))

;; TODO make the resources folder location configurable in some way.

(defn- get-resource-file-path [file-name]
  (Paths/get "content" (into-array String [(subs file-name 0 2) (subs file-name 2 4) (subs file-name 4 6) file-name])))

(defn- get-resource-file [file-name]
  (io/file (str (get-resource-file-path file-name))))

(defn resource-file-exists? [file-name]
  (.exists (get-resource-file file-name)))

(defn create-resource-from-file! [xtdb-node file file-name]
  (let [source-file (if (instance? java.io.File file)
                      file
                      (io/file file))
        checksum (digest/sha-256 source-file)
        ext (if-let [index (cstr/last-index-of file-name ".")]
              (subs file-name 0 index)
              "")
        resource-path (str checksum (if ext "." "") ext)
        resource-file (get-resource-file resource-path)]
    (when-not (.exists resource-file)
      (io/make-parents resource-file)
      (Files/copy (.toPath source-file) (.toPath resource-file) (into-array StandardCopyOption [StandardCopyOption/REPLACE_EXISTING])))
    (let [resource-path (.toPath resource-file)
          doc {:xt/id (random-uuid)
               :created-at (jt/zoned-date-time)
               :mime-type (Files/probeContentType resource-path)
               :checksum checksum
               :location (str resource-path)}]
      (db/execute-tx xtdb-node [[:put-docs :mushin.db/resources
                                 doc]])
      doc)))

(defn get-resource-entry [xtdb-node resource]
  (let [resource-file (if (instance? java.io.File resource)
                        resource
                        (get-resource-file resource))
        checksum (digest/sha-256 resource-file)]
    (first (xt/q xtdb-node (xt/template (-> (from :mushib.db/resources [* {:checksum checksum}])
                                            (limit 1)))))))

(defn has-resource-entry? [xtdb-node resource]
  (let [resource-file (if (instance? java.io.File resource)
                        resource
                        (get-resource-file resource))
        checksum (digest/sha-256 resource-file)]
    (boolean (first (xt/q xtdb-node (xt/template (-> (from :mushib.db/resources [{:checksum checksum}])
                                                     (limit 1))))))))
