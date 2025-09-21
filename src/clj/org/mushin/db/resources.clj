(ns org.mushin.db.resources
  (:require [clj-commons.digest :as digest]
            [clojure.java.io :as io]
            [org.mushin.db.util :as db]
            [java-time.api :as jt]
            [org.mushin.db.timestamps :as timestamps]
            [org.mushin.files :as files]
            [clojure.string :as cstr]
            [xtdb.api :as xt]
            [org.mushin.mime :as mime])
  (:import [java.nio.file Files StandardCopyOption Paths]
           [java.net URI]
           [java.io InputStream File]))

;; TODO make the resources folder location configurable in some way.

(defn valid-uri? [s]
  (try
    (URI. s)
    true
    (catch Exception _
      false)))

(def resources-schema
  {:mushin.db/resources
   [:map {:closed true}
    [:xt/id      :uuid]
    timestamps/created-at
    [:name       :string]
    [:mime-type  :keyword]
    [:location   [:and :string [:fn valid-uri?]]]]})

(defn- get-resource-file-path [file-name]
  (Paths/get "content" (into-array String [(subs file-name 0 2) (subs file-name 2 4) (subs file-name 4 6) file-name])))

(defn- get-resource-file [file-name]
  (io/file (str (get-resource-file-path file-name))))

(defn resource-file-exists? [file-name]
  (.exists (get-resource-file file-name)))


(defn get-resource
  [xtdb-node ^String name]
  (db/lookup-first xtdb-node :mushin.db/resources '[xt/id name] {:name name}))

(defn create-resource-from-file!
  [xtdb-node src-path name mime-type]
  (if-let [existing-resource (get-resource xtdb-node name)]
    existing-resource
    (let [file (io/input-stream (if (string? src-path)
                                  src-path
                                  (str src-path)))
          mime-extension (or (mime/mime-types mime-type) (throw (ex-info "Invalid mime type" {:mime-type mime-type})))
          resource-path (get-resource-file-path (str name "." mime-extension))
          resource-path-str (str resource-path)
          doc {:xt/id (random-uuid)
               :created-at (jt/zoned-date-time)
               :mime-type mime-type
               :name name
               :location resource-path-str}]
      (io/make-parents resource-path-str)
      ;; TODO modify this to support other methods of content saving.
      (files/copy file resource-path)
      (db/execute-tx xtdb-node [[:put-docs :mushin.db/resources doc]])
      (select-keys doc [:xt/id :name]))))

(defn create-resource-from-stream!
  [xtdb-node ^InputStream stream name mime-type]
  (if-let [existing-resource (get-resource xtdb-node name)]
    existing-resource
    (let [mime-extension (or (mime/mime-types mime-type) (throw (ex-info "Invalid mime type" {:mime-type mime-type})))
          resource-path (str (get-resource-file-path (str name "." mime-extension)))
          doc {:xt/id (random-uuid)
               :created-at (jt/zoned-date-time)
               :mime-type mime-type
               :name name
               :location resource-path}]
      (io/make-parents resource-path)
      ;; TODO modify this to support other methods of content saving.
      (with-open [output-file (io/output-stream resource-path)]
        (io/copy stream output-file))
      (db/execute-tx xtdb-node [[:put-docs :mushin.db/resources doc]])
      (select-keys doc [:xt/id :name]))))

(defn get-resource-entry [xtdb-node resource]
  (let [resource-file (if (instance? java.io.File resource)
                        resource
                        (get-resource-file resource))
        checksum (digest/sha-256 resource-file)]
    (first (xt/q xtdb-node (xt/template (-> (from :mushib.db/resources [* {:name checksum}])
                                            (limit 1)))))))

(defn has-resource-entry? [xtdb-node resource]
  (let [resource-file (if (instance? java.io.File resource)
                        resource
                        (get-resource-file resource))
        checksum (digest/sha-256 resource-file)]
    (boolean (first (xt/q xtdb-node (xt/template (-> (from :mushib.db/resources [{:name checksum}])
                                                     (limit 1))))))))
