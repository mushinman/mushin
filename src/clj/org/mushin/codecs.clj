(ns org.mushin.codecs
  (:require [buddy.core.codecs])
  (:import [java.util Base64]))

(defn bytes->b64
  [b]
  (buddy.core.codecs/bytes->b64))

(defn bytes->b64u
  [b]
  (let [decoder Base64/getUrlDecoder]
    (.encodeToString decoder b)))
