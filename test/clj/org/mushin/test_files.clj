(ns org.mushin.test-files
  (:require [org.mushin.files :as sut]
            [clojure.test :as t])
  (:import [java.nio.file Path]))


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
