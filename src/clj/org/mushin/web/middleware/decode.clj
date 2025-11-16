(ns org.mushin.web.middleware.decode
  (:require [org.mushin.codecs :as codecs]
            [clojure.tools.logging :as log]
            [ring.util.http-response :refer [bad-request!]]))

(def ^:private cursor-path [:query-params "cursor"])

(defn encode-cursor
  [{{:keys [cursor]} :body :as result}]
  (cond-> result
    cursor (assoc-in [:body :cursor] (codecs/->b64u-edn cursor))))

(defn wrap-encoded-params
  "Decode special params like cursors."
  [handler]
  (fn [req]
    (let [cursor (get-in req cursor-path)]
      (-> (handler
           (cond-> req
             cursor (assoc-in [:query-params :cursor]
                              (try
                                (codecs/b64u-edn-> cursor)
                                (catch IllegalArgumentException _
                                  (bad-request! {:error :invalid-cursor
                                                 :message "Your cursor was not valid base64-url"}))))))
          encode-cursor))))
