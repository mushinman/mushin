(ns org.mushin.files
  (:import [java.nio.file CopyOption Files Path Paths StandardCopyOption LinkOption]
           [java.nio.file.attribute FileAttribute]
           [java.io File]
           [java.net URI]))

(defn path
  ^Path
  [^String value]
  (Paths/get value (into-array String [])))

(defn move
  ^Path
  [^Path src ^Path dst]
  (Files/move src dst (into-array CopyOption [StandardCopyOption/ATOMIC_MOVE])))

(defn copy
  ^Path
  [^Path src ^Path dst]
  (Files/copy src dst (into-array CopyOption [])))

(defn str->path
  ^Path
  [^String value]
  (path value))

(defn create-temp-file
  ^Path
  [^String prefix ^String extension]
  (Files/createTempFile prefix extension (into-array FileAttribute [])))

(defn delete
  [^Path path]
  (Files/delete path))

(defn delete-if-exists
  ^Boolean
  [^Path path]
  (Files/deleteIfExists path))

(defn path-combine
  [^Path path & others]
  (if (empty? others)
    path
    (recur (.resolve path (first others)) (rest others))))

(defn exists
  [^Path path & options]
  (Files/exists path (into-array LinkOption options)))

(defn not-exists
  [^Path path & options]
  (Files/notExists path (into-array LinkOption options)))

(defn path->uri
  ^URI
  [^Path path]
  (.toUri path))
