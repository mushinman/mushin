(ns org.mushin.codecs
  (:require [buddy.core.codecs]
            [clojure.edn :as edn]
            [java-time.api :as time])
  (:import [java.util Base64]
           [java.util HexFormat]
           [java.nio.charset StandardCharsets]))

(def ^:private charset-utf8 StandardCharsets/UTF_8)

(def ^:private xt-readers
  {'xt/zdt (fn [^String s] (time/zoned-date-time s))})

(defn read-edn
  [s]
  (edn/read-string {:readers (merge default-data-readers xt-readers)} s))

(defn bytes->b64
  "Encode a byte-array to a Base64 string (no padding)."
  ^String
  [^bytes b]
  (-> (Base64/getEncoder)
      (.withoutPadding)
      (.encodeToString b)))

(defn b64->bytes
  "Decode a Base64 string to a byte array."
  ^bytes
  [^String s]
  (-> (Base64/getDecoder)
      (.decode s)))

(defn bytes->b64u
  "Encode a byte-array to a URL-safe Base64 string (no padding)."
  ^String
  [^bytes b]
  (-> (Base64/getUrlEncoder)
      (.withoutPadding)
      (.encodeToString b)))

(defn b64u->bytes
  "Decode a URL-safe Base64 string to a byte array."
  ^bytes
  [^String s]
  (-> (Base64/getUrlDecoder)
      (.decode s)))

(defn ->b64u-edn
  "Transform the input into url-safe Base64 encoded edn."
  ^String
  [o]
  (-> o
      pr-str
      (.getBytes charset-utf8)
      bytes->b64u))

(defn b64u-edn->
  [s]
  (-> s
      b64u->bytes
      (String. charset-utf8)
      read-edn))

(defn hex->bytes
  "Decode a hex string string to a byte array."
  ^bytes
  [^String s]
  (.parseHex (HexFormat/of) s))

(defn bytes->hex
  "Encode a byte-array to a hex string."
  ^String
  [^bytes b]
  (.formatHex (HexFormat/of) b))
