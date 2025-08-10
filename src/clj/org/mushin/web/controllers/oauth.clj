(ns org.mushin.web.controllers.oauth
  (:require [clojure.tools.logging :as log]
            [ring.util.http-response :refer [found ok unauthorized! bad-request!]]
            [buddy.sign.util :as jwt-utils]
            [java-time.api :as time]
            [org.mushin.db.remember-me :as remember-me]
            [lambdaisland.uri :as uri]
            [buddy.sign.jwt :as jwt]
            [org.mushin.digest :as digest]
            [org.mushin.codecs :as codecs]
            [org.mushin.db.util :as db]
            [org.mushin.web.auth-utils :as auth]))

(def create-code-post-body
  [:map {:closed false}
   [:username :string]
   [:password :string]
   [:state :string]
   [:code-method [:enum "S256"]]
   [:code-challenge :string]
   [:redirect-uri :string]])

(def auth-callback-get-query
  [:map {:closed false}
   [:state :string]
   [:code-verifier [:string {:min 43 :max 128}]]
   [:code :string]])

(def token-post-body
  [:or
   [:map
    [:grant-type     [:enum "refresh_token"]]
    [:state          :string]
    [:refresh-token  :string]]
   [:map
    [:grant-type     [:enum "authorization_code"]]
    [:state          :string]
    [:code           :string]
    [:code-verifier  :string]
    [:redirect-uri   :string]]])

(defn get-jwt-value!
  [code key]
  (try (jwt/unsign code key {:alg :eddsa})
       (catch Exception e
         (let [{:keys [type cause]} (ex-data e)]
           (if (and (= type :validation) (= cause :signature))
             (unauthorized! {:error :invalid-jwt-token :message "Your JWT token was invalid"})
             (throw e))))))

(defn create-oauth-tokens!
  [xtdb-node jwks user-id]
  (let [[selector validator valid-for doc] (remember-me/remember-user user-id)
        exp 3600
        issued-at (jwt-utils/now)]
    (db/submit-tx xtdb-node [[:put-docs :mushin.db/remember-me doc]])
    {:token (jwt/sign {:exp exp :iat issued-at :sub user-id :type :oauth-sign-in-code}
                      (:priv jwks)
                      {:alg :eddsa :header {:alg "EDDSA" :type "JWT"}})
     :token-type "bearer"
     :expires-in exp
     :scope "*"
     :refresh (jwt/sign {:exp (time/as valid-for :seconds)
                         :iat issued-at
                         :type :oauth-refresh-token
                         :selector selector
                         :validator validator}
                        (:priv jwks)
                        {:alg :eddsa :header {:alg "EDDSA" :type "JWT"}})}))


(defn create-code-post!
  "Verify user credentials and redirect the user to `oauth/auth-callback` with a code if successful."
  [{:keys [xtdb-node mushin/jwks]}
   {{{:keys [username password state redirect-uri code-method code-challenge]} :body} :parameters}]
  ;; TODO verify the redirect_uri
  (when-not (auth/check-nickname-password xtdb-node username password)
    (auth/failed-auth! {:error :wrong-nickname-or-password :message "the provided nickname or password is incorrect"}))
  (log/info "Issued sign in token" {:event :created-sign-in-token :user username})
  (found (-> (uri/uri redirect-uri)
             (uri/assoc-query {:code (jwt/sign {:exp (+ 120 (jwt-utils/now)) :sub username :type :oauth-sign-in-code :code-method code-method :code-challenge code-challenge}
                                               (:priv jwks)
                                               {:alg :eddsa :header {:alg "EDDSA" :type "JWT"}})
                               :state state})
             (uri/uri-str))))

(defn validate-oauth-code!
  "Validates the oauth `code` using the private keys provided in `jwks`.
  Throws an exception if the code is invalid, otherwise returns the JWK's `sub`."
  [jwks code code-verifier]
  (let [{:keys [code-challenge code-method type exp iat]} (get-jwt-value! code (:pub jwks))]
      ;; In a real oauth implementation we'd call the remote oauth server here, but since we only support
      ;; ourself as the auth server that call isn't necessary.
    (when-not (and (= type :oauth-sign-in-code)
                   (= code-method "S256")
                   (< (jwt-utils/now) (+ iat exp))
                   (digest/eq (codecs/b64u->bytes code-challenge) (digest/sha-256-b64u code-verifier)))
      (unauthorized! {:error :invalid-oauth-code :message "The code you submitted was not valid"}))))

(defn token-post!
  [{:keys [xtdb-node mushin/jwks]}
   {{{:keys [grant-type] :as body} :body} :parameters}]
  ;; TODO verify the redirect-uri
  (ok
    (create-oauth-tokens! xtdb-node jwks
                          (cond
                            (= grant-type "refresh_token")
                            (let [{:keys [code-verifier code]} (:code body)]
                              (validate-oauth-code! jwks code code-verifier))

                            (= grant-type "authorization_code")
                            (let [{:keys [exp iat type selector validator]} (get-jwt-value! (:refresh-token body) jwks)]
                              (when-not (and (= type :oauth-refresh-token)
                                             (< (jwt-utils/now) (+ iat exp)))
                                (unauthorized! {:error :invalid-remember-me :module :oauth :message "Your refresh token is invalid"}))
                              (if-let [token (remember-me/recall-user xtdb-node selector validator)]
                                (let [{:keys [user-id xt/id]} token]
                                  (db/submit-tx xtdb-node [[:erase-docs :mushin.db/remember-me id]]) ; Delete the old token.
                                  user-id)
                                (unauthorized! {:error :invalid-remember-me :module :oauth :message "Your refresh token is invalid"})))

                            :else (bad-request! {:error :invalid-oauth-type :message "The oauth type requested is not supported by the server"})))))

(defn auth-callback-get
  "Authorize the user based off their provided username and password. Verifies the PKCE code challenge.
  Replies with an access code that the client can redeem for a bearer token at `/api/oauth/token`."
  [{:keys [mushin/jwks xtdb-node]}
   {{{:keys [code code-verifier]} :query} :parameters}]
  ;; TODO this call is supposed to make a HTTP request to the real oauth server, which I should probably do
  ;; instead of just pretending that this is a real implementation.
  (ok (create-oauth-tokens! xtdb-node jwks (validate-oauth-code! jwks code code-verifier))))

;; TODO we need to echo back the state param on both success and error. That's probably most easily done with a middleware.
;; TODO we should add a cache-control no-store middleware too.
