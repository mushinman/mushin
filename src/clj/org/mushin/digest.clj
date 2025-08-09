(ns org.mushin.digest
  (:require [clj-commons.digest :as clj-digest]
            [org.mushin.codecs :as codecs])
  (:import [java.security MessageDigest]))

(defn sha-256-bytes
  [^bytes data]
  (-> (clj-digest/sha-256 data)
      (codecs/hex->bytes)))

(defn sha-256-b64u
  [^bytes data]
  (-> (clj-digest/sha-256 data)
      (codecs/hex->bytes)
      (codecs/bytes->b64u)))

(defn sha-256-b64
  [^bytes data]
  (-> (clj-digest/sha-256 data)
      (codecs/hex->bytes)
      (codecs/bytes->b64)))

(defn eq
  "Compare two bytestreams in constant time."
  [^bytes b1 ^bytes b2]
  (MessageDigest/isEqual b1 b2))
