(ns org.mushin.test-support.db
  (:require [clojure.java.io :as io]
            [org.mushin.test-files :as files]
            [xtdb.node :as xn])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
           [java.io Closeable]))

(defrecord TestXtdbNode
    [node root]
  Closeable
  (close [_]
    (try (.close node) (catch Throwable _))
    (try (when root
           (let [f (.toFile root)]
             ;; best-effort recursive delete
             (when (.exists f)
               (run! #(when (.exists %) (.delete %))
                     (reverse (file-seq f)))
               (.delete f))))
         (catch Throwable _))))

(defn- tmp-dir
  "Create and return a java.nio.file.Path for a unique temp dir."
  ([] (tmp-dir "xtdb-test-"))
  ([prefix]
   (Files/createTempDirectory prefix (make-array FileAttribute 0))))

(defn start-xtdb!
  "Start a throwaway XTDB node under a unique temp directory."
  []
  (let [root (tmp-dir)
        node (xn/start-node {:log
                             ;; persistent transaction-log under data/xtdb/log
                             [:local
                              {:path (str (files/path-combine root "data/xtdb/log"))}]

                             :storage
                             ;; persistent object store under data/xtdb/objects
                             [:local
                              {:path (str (files/path-combine root "data/xtdb/objects"))}]} )]
    (TestXtdbNode. node root)))
