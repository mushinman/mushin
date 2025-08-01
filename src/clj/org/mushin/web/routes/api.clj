(ns org.mushin.web.routes.api
  (:require
    [org.mushin.web.controllers.health :as health]
    [org.mushin.web.controllers.auth :as auth-handlers]
    [org.mushin.web.middleware.exception :as exception]
    [org.mushin.web.middleware.formats :as formats]
    [integrant.core :as ig]
    [org.mushin.web.middleware.auth :as auth]
    [reitit.coercion.malli :as malli]
    [reitit.ring.coercion :as coercion]
    [reitit.ring.middleware.muuntaja :as muuntaja]
    [reitit.ring.middleware.parameters :as parameters]
    [org.mushin.web.controllers.accounts :as accounts]
    [ring.logger :as ring-logger]
    [org.mushin.web.middleware.tx-func :as tx]
    [org.mushin.web.controllers.statuses :as statuses]
    [reitit.swagger :as swagger]))

(def route-data
  {:coercion   malli/coercion
   :muuntaja   formats/instance
   :swagger    {:id ::api}
   :middleware [;; query-params & form-params
                ring-logger/wrap-log-response
                  ;; Logging
                parameters/parameters-middleware
                  ;; content-negotiation
                muuntaja/format-negotiate-middleware
                  ;; Async headers
                tx/wrap-add-tx-fn
                  ;; encoding response body
                muuntaja/format-response-middleware
                  ;; exception handling
                coercion/coerce-exceptions-middleware
                  ;; decoding request body
                muuntaja/format-request-middleware
                  ;; coercing response bodys
                coercion/coerce-response-middleware
                  ;; coercing request parameters
                coercion/coerce-request-middleware
                  ;; exception handling
                exception/wrap-exception]})
;; Routes
(defn api-routes [opts]
  [["/swagger.json"
    {:get {:no-doc  true
           :swagger {:info {:title "org.mushin API"}}
           :handler (swagger/create-swagger-handler)}}]
   ["/health"
    ;; note that use of the var is necessary
    ;; for reitit to reload routes without
    ;; restarting the system
    {:get #'health/healthcheck!}]
   ["/status"
    {:get {:handler (partial health/db-status opts)}}]
   ["/status-q"
    {:get {:handler (partial health/dq-texas opts)}}]
   ["/patch-test"
    {:get {:handler (partial health/dbq-patch opts)}}]
   ["/patch-test2"
    {:get {:handler (partial health/dbq-patch-incomplete opts)}}]
   ["/auth-test"
    {:get  {:handler (partial health/auth-test-post opts)
            :middleware [(partial auth/wrap-authenticate-user opts)]}}]
   ["/login" {:handler (partial auth-handlers/login! opts)}]
   ["/logout" {:handler auth-handlers/logout!}]
   ["/create-account"
    {:post {:handler (partial accounts/create-account-post! opts)
            :parameters {:body accounts/create-account-body}}}]
   ["/statuses/timeline/:nickname"
    {:get  {:handler (partial statuses/get-timeline opts)
            :middleware [(partial auth/wrap-authenticate-user opts)]
            :parameters {:query statuses/get-timeline-query}}}]
   ;["/create-picture" {:handler (partial statuses/create-picture-post! opts)
   ;                    :middleware [(partial auth/wrap-authenticate-user opts)]
   ;                    :parameters {:body statuses/create-picture-post-body}}]

   ["/create-status-post" {:handler (partial statuses/create-status-post! opts)
                         :middleware [(partial auth/wrap-authenticate-user opts)]
                         :parameters {:body statuses/create-status-body}}]

   ["/statuses/s/:id" {:get  {:handler (partial statuses/get-status opts)
                              :middleware [(partial auth/wrap-authenticate-user opts)]
                              :parameters {:query statuses/get-status-query
                                           :path statuses/status-query}}
                       :delete {:handler (partial statuses/delete-status! opts)
                                :middleware [(partial auth/wrap-authenticate-user opts)]
                                :parameters {:query statuses/get-status-query
                                             :path statuses/status-query}}}]])

(derive :reitit.routes/api :reitit/routes)

(defmethod ig/init-key :reitit.routes/api
  [_ {:keys [base-path]
      :or   {base-path ""}
      :as   opts}]
  (fn [] [base-path route-data (api-routes opts)]))
