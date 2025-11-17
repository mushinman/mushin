(ns org.mushin.web.routes.api
  (:require
   [org.mushin.web.controllers.health :as health]
   [org.mushin.web.controllers.auth :as auth-handlers]
   [org.mushin.web.middleware.exception :as exception]
   [org.mushin.web.middleware.formats :as formats]
   [org.mushin.web.middleware.state :as state]
   [org.mushin.web.middleware.decode :as decode]
   [integrant.core :as ig]
   [ring.util.response :as resp]
   [org.mushin.web.middleware.auth :as auth]
   [org.mushin.web.middleware.cache-control :as cache-control]
   [reitit.coercion.malli :as malli]
   [reitit.ring.coercion :as coercion]
   [reitit.ring.middleware.muuntaja :as muuntaja]
   [reitit.ring.middleware.parameters :as parameters]
   [org.mushin.web.controllers.oauth :as oauth]
   [org.mushin.web.controllers.users :as users]
   [org.mushin.web.controllers.i :as self]
   [ring.logger :as ring-logger]
   [org.mushin.web.middleware.tx-func :as tx]
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
                ;; Decode special params
                decode/wrap-encoded-params
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
   ["/oauth"
    ["/callback"
     ["/login"
      {:get {:handler (partial oauth/auth-callback-get opts)
             :parameters {:query oauth/auth-callback-get-query}
             :middleware [(partial cache-control/wrap-cache-control "no-store")]}}]]

    ["/token"
     {:post {:handler (partial oauth/token-post! opts)
             :parameters {:body oauth/token-post-body}
             :middleware [(partial cache-control/wrap-cache-control "no-store")
                          state/wrap-state]}}]
    ["/authorize"
     {:get {:handler (fn [_] (resp/resource-response "authorization.html" {:root "public"}))}}]
    ["/create-code"
     {:post {:handler (partial oauth/create-code-post! opts)
             :parameters {:body oauth/create-code-post-body}}}]
    ["/auth-callback"
     {:post {:handler (partial oauth/create-code-post! opts)
             :parameters {:body oauth/create-code-post-body}
             :middleware [state/wrap-state]}}]]

   ["/v1"
    ["/are-you-ok"
     {:get {:handler health/check}}] ; TODO no-cache response header

    ["/authn"
     ["/recall"
      {:post {:handler (partial auth-handlers/refresh-session! opts)}}]
     ["/let-me-in"
      {:post {:handler (partial auth-handlers/login! opts)}}]
     ["/bye"
      {:post {:handler (partial auth-handlers/logout! opts)}}]]

    ["/i"
     {:middleware [(partial auth/wrap-authenticate-user opts)]}
     ["/who-am-i"
      {:get {:handler (partial self/get-user opts)}}]
     ;; TODO authorization middleware that checks the arguments and denies based off
     ["/delete-me"
      {:delete {:handler (partial self/delete-self! opts)
                :parameters {:body self/delete-me-body-schema}}}]
     ;; ["/statuses/:id"
     ;;  {}
     ;;  ["/redact"]
     ;;  ["/like"]
     ;;  ["/unlike"]
     ;;  ["/favor"]
     ;;  ["/unfavor"]]

     ["/relationships"

      ["/following"
       {:get {:handler (partial self/get-following opts)
              :parameters {:query (self/collection-query-schema)}}}]
      ["/followers"
       {:get {:handler (partial self/get-followers opts)
              :parameters {:query (self/collection-query-schema)}}}]
      ["/blocks"
       {:get {:handler (partial self/get-blocked-accounts opts)
              :parameters {:query (self/collection-query-schema)}}}]
      ["/mutes"
       {:get {:handler (partial self/get-muted-accounts opts)
              :parameters {:query (self/collection-query-schema)}}}]

      ["/:id"
       ["/follow"
        {:post {:handler (partial self/follow-user! opts)
                :parameters {:path [:map [:id :uuid]]}}}]

       ["/unfollow"
        {:post {:handler (partial self/unfollow-user! opts)
                :parameters {:path [:map [:id :uuid]]}}}]
       ["/mute"
        {:post {:handler (partial self/mute-user! opts)
                :parameters {:path [:map [:id :uuid]]}}}]
       ["/unmute"
        {:post {:handler (partial self/unmute-user! opts)
               :parameters {:path [:map [:id :uuid]]}}}]
       ["/block"
        {:post {:handler (partial self/block-user! opts)
               :parameters {:path [:map [:id :uuid]]}}}]
       ["/unblock"
        {:post {:handler (partial self/unblock-user! opts)
               :parameters {:path [:map [:id :uuid]]}}}]]]]

    ["/users"
     ["/create"
      {:post {:handler (partial users/create-user! opts)
              :parameters {:body users/create-user-body}}}]]


    ;; ["/statuses/timeline/:nickname"
    ;;  {:get  {:handler (partial statuses/get-timeline opts)
    ;;          :middleware [(partial auth/wrap-authenticate-user opts)]
    ;;          :parameters {:query statuses/get-timeline-query}}}]
                                        ;["/create-picture" {:handler (partial statuses/create-picture-post! opts)
                                        ;                    :middleware [(partial auth/wrap-authenticate-user opts)]
                                        ;                    :parameters {:body statuses/create-picture-post-body}}]

    ;; ["/create-status-post" {:handler (partial statuses/create-status-post! opts)
    ;;                         :middleware [(partial auth/wrap-authenticate-user opts)]
    ;;                         :parameters {:body statuses/create-status-body}}]

    ;; ["/statuses/s/:id" {:get  {:handler (partial statuses/get-status opts)
    ;;                            :middleware [(partial auth/wrap-authenticate-user opts)]
    ;;                            :parameters {:query statuses/get-status-query
    ;;                                         :path statuses/status-query}}
    ;;                     :delete {:handler (partial statuses/delete-status! opts)
    ;;                              :middleware [(partial auth/wrap-authenticate-user opts)]
    ;;                              :parameters {:query statuses/get-status-query
    ;;                                           :path statuses/status-query}}}]
    ]])

(derive :reitit.routes/api :reitit/routes)

(defmethod ig/init-key :reitit.routes/api
  [_ {:keys [base-path]
      :or   {base-path "api"}
      :as   opts}]
  (fn [] [base-path route-data (api-routes opts)]))
