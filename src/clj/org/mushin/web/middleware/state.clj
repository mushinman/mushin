(ns org.mushin.web.middleware.state)

(defn wrap-state
  [handler]
  (fn [req]
    (if-let [state (or (get-in req [:parameters :query :state]) (get-in req [:parameters :body :state]))]
      (try
        (handler req)
       (catch Exception e
         (let [{:keys [type response]} (ex-data e)]
           (if (= type :ring.util.http-response/response)
             (assoc-in response [:response :body :state] state)
             (throw (Exception. "Exception thrown in request handler" e))))))
      (handler req))))
