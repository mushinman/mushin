(ns org.mushin.digest
  (:require [clj-commons.digest :as clj-digest]
            [org.mushin.codecs :as codecs])
  (:import [java.security MessageDigest]))

(defn create-sha256-digest
  []
  (MessageDigest/getInstance "SHA-256"))

(defn update-digest-byte
  [^MessageDigest md ^Byte b]
   (.update md b))

(defn update-digest-buffer
  ([^MessageDigest md ^bytes data]
   (.update md data))
  ([^MessageDigest md ^bytes data ^long offset ^long length]
   (.update md data offset length)))

(defn sha-256-bytes
  [data]
  (-> (clj-digest/sha-256 data)
      (codecs/hex->bytes)))

(defn sha-256-b64u
  [data]
  (-> (clj-digest/sha-256 data)
      (codecs/hex->bytes)
      (codecs/bytes->b64u)))

(defn sha-256-b64
  [data]
  (-> (clj-digest/sha-256 data)
      (codecs/hex->bytes)
      (codecs/bytes->b64)))

(defn digest->bytes
  [^MessageDigest md]
  (.digest md))

(defn digest->b64
  [^MessageDigest md]
  (-> (.digest md)
      (codecs/bytes->b64)))

(defn digest->b64u
  [^MessageDigest md]
  (-> (.digest md)
      (codecs/bytes->b64u)))

(defn eq
  "Compare two bytestreams in constant time."
  [^bytes b1 ^bytes b2]
  (MessageDigest/isEqual b1 b2))
