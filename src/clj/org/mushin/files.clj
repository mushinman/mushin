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
  [^Path src ^Path dst & copy-options]
  (Files/copy src dst (into-array CopyOption copy-options)))

(defn get-path
  ^String
  [^File file]
  (.getPath file))

(defn probe-content-type
  ^String
  [^Path file-path]
  (Files/probeContentType file-path))

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
  "Concatenate the path with the provided path parts.

  # Arguments
  - `path`: The base path
  - `others`: A sequence of path parts.

  # Return value
  `path` concatenated with `others`."
  ^Path
  [^Path path & others]
  (if (empty? others)
    path
    (recur (.resolve path (first others)) (rest others))))

(defn to-absolute-path
  ^Path
  [^Path p]
  (.toAbsolutePath p))

(defn to-uri
  ^URI
  [^Path p]
  (.toUri p))

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
