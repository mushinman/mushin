(ns org.mushin.web.controllers.oauth
  (:require [clojure.tools.logging :as log]
            [ring.util.http-response :refer [found ok unauthorized!]]
            [buddy.sign.util :as jwt-utils]
            [lambdaisland.uri :as uri]
            [buddy.sign.jwt :as jwt]
            [org.mushin.web.auth-utils :as auth]))

(def create-code-post-body
  [:map {:closed false}
   [:username :string]
   [:password :string]
   [:state :string]
   [:redirect-uri :string]])

(def auth-callback-query
  [:map {:closed false}
   [:state :string]
   [:code :string]])

(def token-body
  [:map
   [:grant-type [:enum "refresh_token" "authorization_code"]]
   [:refresh-token {:optional true} :string]
   [:code {:optional true} :string]
   [:redirect-uri {:optional true} :string]])

(defn auth-callback-get
  "Authorize the user based off their provided username and password. Replies with an access code that the client
  can redeem for a bearer token at `/api/oauth/token`."
  [{:keys [mushin/jwks]}
   {{{:keys [state code]} :query} :parameters}]
  ;; TODO check state
      ;; In a real oauth implementation we'd call the remote oauth server here, but since we only support

  )

(defn create-code-post!
  "Verify user credentials and redirect the user to `oauth/auth-callback` with a code if successful."
  [{:keys [xtdb-node mushin/jwks]}
   {{{:keys [username password state redirect-uri]} :body} :parameters}]
  ;; TODO check state
  (when-not (auth/check-nickname-password xtdb-node username password)
    (auth/failed-auth! {:error :wrong-nickname-or-password :message "the provided nickname or password is incorrect"}))
  (log/info "Issued sign in token" {:event :created-sign-in-token :user username})
  (found (-> (uri/uri redirect-uri)
             (uri/assoc-query {:code (jwt/sign {:exp (+ 120 (jwt-utils/now)) :sub username :type :sign-in-code}
                                               (:priv jwks)
                                               {:alg :eddsa :header {:alg "EDDSA" :type "JWT"}})
                               :state state})
             (uri/uri-str))))

(defn token-post!
  [{:keys [xtdb-node mushin/jwks]}
   {{{:keys [state code]} :query} :parameters}]
  (let [{:keys [exp type username]} (try (jwt/unsign code {:pub jwks} {:alg :eddsa})
                                         (catch Exception e
                                           (let [{:keys [type cause]} (ex-data e)]
                                             (if (and (= type :validation) (= cause :signature))
                                               (unauthorized! {:error :invalid-jwt-token :message "Your JWT token was invalid"})
                                               (throw e)))))
        exp (* 10 60)]
    (when-not (= type :sign-in-code)
      (unauthorized! {:error :wrong-code-type :message "Your code is not an oauth code"}))
    (if (> (jwt-utils/now) (+ exp 120))
      (unauthorized! {:error :expired-code :message "Your oauth login code has expired"})
      (ok {:token (jwt/sign {:exp (+ exp (jwt-utils/now)) :sub username :type :bearer}
                            (:priv jwks)
                            {:alg :eddsa :header {:alg "EDDSA" :type "JWT"}})
           :expires-at exp
           :refresh (jwt/sign {:exp (+ exp (jwt-utils/now)) :sub username :type :bearer}
                              (:priv jwks)
                              {:alg :eddsa :header {:alg "EDDSA" :type "JWT"}})}))))
