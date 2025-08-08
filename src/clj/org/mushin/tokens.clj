(ns org.mushin.tokens
  (:require [buddy.core.nonce :as nonce]
            [buddy.core.codecs :as codecs]))

(defn generate-token
  [n-bytes]
  (-> (nonce/random-bytes n-bytes)
      (codecs/bytes->b64u)))
