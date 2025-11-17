(ns org.mushin.files
  (:import [java.nio.file CopyOption Files Path Paths LinkOption]
           [java.nio.file.attribute FileAttribute]
           [java.nio.charset StandardCharsets]
           [java.io File OutputStream InputStream]
           [java.net URI]))

(def charset-utf8 StandardCharsets/UTF_8)

(def tmp-dir (System/getProperty "java.io.tmpdir"))

(defn sanitize-file
  [file]
  (if (instance? Path file)
    (str file)
    file))

(defn null-output-stream
  ^OutputStream
  []
  (OutputStream/nullOutputStream))

(defn transfer-to
  ^long
  [^InputStream input-stream ^OutputStream output-stream]
  (.transferTo input-stream output-stream))

(defn path
  "Create a path from a base path and list of path parts.


  # Arguments
  - `base`: The base of the path.
  - `parts`: Additional parts to concatenate onto base."
  ^Path
  [^String base & parts]
  (Paths/get base (into-array String parts)))

(defn to-real-path
  [^Path p & options]
  (.toRealPath p (into-array LinkOption options)))

(defn normalize
  [^Path p]
  (.normalize p))

(defn move
  "Move a file from path `src` to `dst`.

  # Arguments
  - `src`: A path to the source file.
  - `dst`: A path to the file's destination.
  - `copy-options`: An optional list of copy options.

  `copy-options` can include any of the following options:


  | Option                                              | Description                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
  |-----------------------------------------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
  | `java.nio.file.StandardCopyOption.REPLACE_EXISTING` | If the target file exists, then the target file is replaced if it is not a non-empty directory. If the target file exists and is a symbolic link, then the symbolic link itself, not the target of the link, is replaced.                                                                                                                                                                                                                                                                                                                                             |
  | `java.nio.file.StandardCopyOption.ATOMIC_MOVE`      | The move is performed as an atomic file system operation and all other options are ignored. If the target file exists then it is implementation specific if the existing file is replaced or this method fails by throwing an IOException. If the move cannot be performed as an atomic file system operation then AtomicMoveNotSupportedException is thrown. This can arise, for example, when the target location is on a different FileStore and would require that the file be copied, or target location is associated with a different provider to this object. |


  # Return
  The path to the target file."
  ^Path
  [^Path src ^Path dst & copy-options]
  (Files/move src dst (into-array CopyOption copy-options)))

(defn copy
  "Copy a file path at `src` to `dst`.

  # Arguments
  - `src`: A path to the source file.
  - `dst`: A path to the file's destination.
  - `copy-options`: An optional list of copy options.

  `copy-options` can include any of the following options:

  | Option                                              | Description |
  |-----------------------------------------------------|-------------|
  | `java.nio.file.StandardCopyOption.REPLACE_EXISTING` | If the target file exists, then the target file is replaced if it is not a non-empty directory. If the target file exists and is a symbolic link, then the symbolic link itself, not the target of the link, is replaced.
  | `java.nio.file.StandardCopyOption.COPY_ATTRIBUTES`  | Attempts to copy the file attributes associated with this file to the target file. The exact file attributes that are copied is platform and file system dependent and therefore unspecified. Minimally, the last-modified-time is copied to the target file if supported by both the source and target file stores. Copying of file timestamps may result in precision loss.   |
  | `java.nio.file.StandardCopyOption.NOFOLLOW_LINKS`   | Symbolic links are not followed. If the file is a symbolic link, then the symbolic link itself, not the target of the link, is copied. It is implementation specific if file attributes can be copied to the new link. In other words, the COPY_ATTRIBUTES option may be ignored when copying a symbolic link.  |
  # Return
  The path to the target file.
  "
  ^Path
  [^Path src ^Path dst & copy-options]
  (Files/copy src dst (into-array CopyOption copy-options)))

(defn get-path
  "Get the path name of the file.

  # Arguments
  `file`: The file to get the file path to.

  # Return
  The path to the provided file."
  ^String
  [^File file]
  (.getPath file))

(defn probe-content-type
  "Probes the content type (mime type) of a file.

  # Arguments
  - `file-path`: The path to the file to probe for content type.

  # Return
  The mime type of the file at `file-path`, or nil if it could not be determined."
  ^String
  [file-path]
  (Files/probeContentType (path (str file-path))))


(defn create-temp-file
  "Create a temp file.

  # Arguments
  - `prefix`: A prefix to prepend to the temp file's name.
  - `suffix`: A suffix to append to the temp file's name.
  - `attrs`: (Optional) list of file open attributes to open the file with.

  This function can be called with no arguments, in which case we let the JVM
  decide the file name and attributes.

  # Return
  A path to the temp file.
  "
  ^Path
  ([^String prefix ^String extension & file-attributes]
   (Files/createTempFile prefix extension (into-array FileAttribute file-attributes)))
  ([]
   (create-temp-file "" "")))

(defn create-temp-file-in
  "Create a temp file.

  # Arguments
  - `dir`:  The directory to place the temp file in.
  - `prefix`: A prefix to prepend to the temp file's name.
  - `suffix`: A suffix to append to the temp file's name.
  - `attrs`: (Optional) list of file open attributes to open the file with.

  # Return
  A path to the temp file.
  "
  ^Path
  [^Path dir ^String prefix ^String extension & file-attributes]
  (Files/createTempFile dir prefix extension (into-array FileAttribute file-attributes)))

(defn delete
  "Deletes a file.

  # Arguments.
  - `path`: The path to the file to delete.

  # Returns
  `nil`"

  [^Path path]
  (Files/delete path))

(defn delete-if-exists
  "Deletes a file if it exists.
  # Arguments.
  - `path`: The path to the file to delete.
  # Returns
  `true` if a file was deleted, `false` otherwise."
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
    (recur (.resolve path ^String (first others)) (rest others))))

(defn to-absolute-path
  "Convert a path to absolute path.

  # Arguments
  - `p` The path to convert to an absolute path.

  # Return
  The path `p` as an absoute path."
  ^Path
  [^Path p]
  (.toAbsolutePath p))

(defn path->uri
  "Convert a path to URI.
  # Arguments
  - `p` The path to convert to a URI.

  # Return
  The path `p` as a URI."
  ^URI
  [^Path p]
  (.toUri p))

(defn exists
  "Tests whether a file exists.

  # Arguments
  - `path`: The path to test the existance of.
  - `options`: An optional list of link options to test the file with.

  # Return
  `true` if the file exists; `false` if the file does not exist or its existance could not be determined."
  [^Path path & options]
  (Files/exists path (into-array LinkOption options)))

(defn not-exists
  "Tests whether a file does not exist.

  # Arguments
  - `path`: The path to test the existance of.
  - `options`: An optional list of link options to test the file with.

  # Return
  `false` if the file exists; `true` if the file does not exist or its existance could not be determined."
  [^Path path & options]
  (Files/notExists path (into-array LinkOption options)))

