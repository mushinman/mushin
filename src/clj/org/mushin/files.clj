(ns org.mushin.files
  (:import [java.nio.file CopyOption Files Path Paths StandardCopyOption]
           [java.nio.file.attribute FileAttribute]
           [java.io File]))

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
  [^String path]
  (Paths/get path (into-array String [])))

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

(create-temp-file "" "")
