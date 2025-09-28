(ns org.mushin.mime
  (:require [clojure.set :as set]))


(def mime-type-to-extensions {"image/png"     "png"
                              "image/jpeg"    "jpg"
                              "image/gif"     "gif"
                              "image/svg+xml" "svg"})

(def extensions-to-mime-type (set/map-invert mime-type-to-extensions))
