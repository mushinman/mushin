(ns org.mushin.codecs
  (:require [buddy.core.codecs])
  (:import [java.util Base64]
           [java.util HexFormat]))

(defn bytes->b64
  "Encode a byte-array to a Base64 string (no padding)."
  [^bytes b]
  (-> (Base64/getEncoder)
      (.withoutPadding)
      (.encodeToString b)))

(defn b64->bytes
  "Decode a Base64 string to a byte array."
  [^String s]
  (-> (Base64/getDecoder)
      (.decode s)))

(defn bytes->b64u
  "Encode a byte-array to a URL-safe Base64 string (no padding)."
  [^bytes b]
  (-> (Base64/getUrlEncoder)
      (.withoutPadding)
      (.encodeToString b)))

(defn b64u->bytes
  "Decode a URL-safe Base64 string to a byte array."
  [^String s]
  (-> (Base64/getUrlDecoder)
      (.decode s)))

(defn hex->bytes
  "Decode a hex string string to a byte array."
  [^String s]
  (.parseHex (HexFormat/of) s))

(defn bytes->hex
  "Encode a byte-array to a hex string."
  [^bytes b]
  (.formatHex (HexFormat/of) b))
