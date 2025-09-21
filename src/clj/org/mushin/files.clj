(ns org.mushin.files
  (:import [java.nio.file Files Path StandardCopyOption]
           [java.io File]))

(defn move
  [^Path src ^Path dst]
  (Files/move src dst StandardCopyOption/ATOMIC_MOVE))

(defn copy
  [^Path src ^Path dst]
  (Files/copy src dst StandardCopyOption/ATOMIC_MOVE))


(defn create-temp-file
  ^Path
  [^String prefix ^String extension]
  (Files/createTempFile prefix extension nil))

(defn delete
  [^Path path]
  (Files/delete path))
