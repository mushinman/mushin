(ns org.mushin.web.controllers.i
  (:require [ring.util.http-response :refer [bad-request! not-found! accepted
                                             no-content unauthorized! conflict! ok]]
            [org.mushin.db.authorization :as db-authz]
            [org.mushin.acc :as acc]
            [malli.experimental.time :as mallt]
            [org.mushin.hiccough :as h]
            [hiccup2.core :as hiccup]
            [lambdaisland.uri :refer [join]]
            [org.mushin.db.media :as media]
            [org.mushin.multimedia.img :as img]
            [org.mushin.multimedia.gif :as gif]
            [org.mushin.resources.resource-map :as res-map]
            [org.mushin.db.resource-meta :as res-meta]
            [org.mushin.multimedia.svg :as svg]
            [org.mushin.files :as files]
            [clojure.tools.logging :as log]
            [org.mushin.db.likes :as likes]
            [xtdb.api :as xt]
            [org.mushin.db.util :as db]
            [org.mushin.db.timeline :as db-timeline]
            [org.mushin.db.relationship :as rel]
            [org.mushin.db.users :as users]
            [org.mushin.db.statuses :as statuses]
            [java-time.api :as time])
  (:import [java.net URI]))

(def any-timestamp
  "A schema for any date object that xtdb supports."
  [:or
   (mallt/-zoned-date-time-schema)
   (mallt/-instant-schema)]) ; TODO Maybe we can support more types with a value transformer...

(defn collection-query-schema
  "Create a schema for api calls that return collections."
  [& {:keys [n-max]
      :or {n-max 20}}]
  [:map
   [:cursor {:optional true}
    [:multi {:dispatch (fn [{:keys [last]}] (first last))}
     [:at
      [:map
       [:cursor-id :uuid]
       [:last [:tuple :keyword any-timestamp]]]]
     [:nickname
      [:map
       [:cursor-id :uuid]
       [:last [:tuple :keyword :string]]]]]]
   [:reverse      {:optional true} :boolean]
   [:order-column {:optional true} [:enum :created-at :name]]
   [:n            {:optional true} [:int {:min 0 :max n-max}]]])


(def bitemporal-query-schema
  "Schema for bitemporal queries."
  [:map
   [:order-column {:optional true} [:enum :valid-from :valid-to :system-from :system-to]]
   [:valid {:optional true} [:or
                             [:map [:at any-timestamp]]
                             [:map [:in [:tuple any-timestamp any-timestamp]]]
                             [:map [:from any-timestamp]]
                             [:map [:to any-timestamp]]]]
   [:system {:optional true} [:or
                              [:map [:at any-timestamp]]
                              [:map [:in [:tuple any-timestamp any-timestamp]]]
                              [:map [:from any-timestamp]]
                              [:map [:to any-timestamp]]]]])

(def check-css-color-schema
  [:re #"^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$"])

(def statuses-body-schema
  "Schema for creating statuses."
  [:map
   [:reply-to {:optional true} :uuid]
   [:payload
    [:multi {:dispatch :type}
     [:comic
      [:map
       [:type     :keyword]
       [:content  :any]]]
     [:microblog
      [:map
       [:type    :keyword]
       [:content :any]]]
     [:meme
      [:map
       [:type           :keyword]
       [:base-img       :string]
       [:alt  :string]
       [:caption 
        [:map
         [:background  check-css-color-schema]
         [:foreground  check-css-color-schema]
         [:font-size   :int]
         [:caption-size :int]
         [:font-family :string]
         [:lines
          [:vector
           [:map
            [:text   :string]
            [:x      :int]
            [:y      :int]]]]]]]]]]])

(def repub-post
  [:status-id                 :uuid])

(def media-schema
  "Schema for uploading media files."
  [:any {:description "mulitpart file"}])

(def bitemporal-collection-query-schema
  "Schema for bitemporal queries on collections."
  [:or
   bitemporal-query-schema
   (collection-query-schema)])

(def delete-me-body-schema
  [:map
   [:password :string]])

(defn get-user
  [{:keys [xtdb-node]}
   {{:keys [user-id]} :session}]
  (if-let [user-doc
           (users/get-user-by-id
            xtdb-node
            user-id)]
    (ok user-doc)
    (not-found! {:error :user-not-found
                 :message "The user you tried to find was not found."})))

(defn get-roles ; TODO
  [{:keys [xtdb-node]}
   {{:keys [user-id]} :session}]
  (db-authz/actor-roles xtdb-node user-id))

(defn delete-self!
  [{:keys [xtdb-node]}
   {{{:keys [password]} :body} :parameters
    {:keys [user-id]} :session
    :keys [mushin/async?]}]
  ;; TODO also invalidate the session state for the deleted user.
  (log/info {:event :delete-me :user-id user-id})
  (let [nick (:nickname (users/get-user-by-id xtdb-node user-id))
        delete-tx [(users/delete-user-tx user-id)
                   (statuses/delete-users-statuses-tx user-id)]]
    (when-not (users/check-nickname-and-password xtdb-node nick password)
      (unauthorized! {:error :invalid-password}))
    (if async?
      (do
        (db/compose-and-submit-txs! xtdb-node delete-tx)
        (accepted))
      (do
        (db/compose-and-execute-txs! xtdb-node delete-tx)
        (no-content)))))


(defn submit-and-accept
  [xtdb-node txs loc]
  (db/compose-and-submit-txs! xtdb-node txs)
  (accepted loc))

(defn exec-and-ok
  ([xtdb-node txs response]
   (db/compose-and-execute-txs! xtdb-node txs)
   (ok response))
  ([xtdb-node txs]
   (db/compose-and-execute-txs! xtdb-node txs)
   (ok)))

(defn- get-user-by-name
  [db-con user-name]
  (users/get-user-by-name db-con user-name))

(defn- get-res-uri
  [db-con resource-id]
  (:location (res-meta/get-resource-by-id db-con resource-id)))

(defn- check-sanitization
  [o]
  (if (map? o)
    (case (:reason o)
      (:character-count-execeeded :invalid-hiccough)
      (bad-request! {:error (:reason o)}))
    o))

(defn create-status!
  [{:keys [xtdb-node endpoint resource-map]}
   {{{{:keys [type] :as payload} :payload :keys [reply-to]} :body} :parameters
    {:keys [user-id]} :session
    :keys [mushin/async?]}]
  (when (and reply-to
             (not (statuses/living-status? xtdb-node type)))
    (bad-request! {:error :status-does-not-exist}))
  (let [{:keys [xt/id ap-id] :as status}
        (case type
          :comic 
          (let [{:keys [content]} payload
                [{:keys [mentions character-count resource-cache]} [comic svg-doc]]
                (check-sanitization
                 (h/sanitize-comic-hiccough
                  content
                  false
                  (partial get-user-by-name xtdb-node)
                  (partial get-res-uri xtdb-node)))
                

                {:keys [ap-id]} (users/get-user-by-id xtdb-node user-id)]
            (statuses/create-status
             user-id
             {:hiccup comic
              :svg (svg/doc->string svg-doc)}
             :comic :svg
             (join ap-id "./statuses/")
             (into #{} (keys resource-cache))
             character-count
             :mentions (into #{} (map :xt/id (vals mentions)))
             :reply-to reply-to))

          :microblog
          (let [{:keys [content]} payload
                [{:keys [mentions character-count resource-cache]} blog]
                (check-sanitization
                 (h/sanitize-microblog-hiccough
                  content
                  false
                  (partial get-user-by-name xtdb-node)
                  (partial get-res-uri xtdb-node)))

                {:keys [ap-id]} (users/get-user-by-id xtdb-node user-id)]
            (statuses/create-status
             user-id
             {:hiccup blog
              :html (str (hiccup/html blog))}
             :microblog :hiccup (join ap-id "./statuses/")
             (into #{} (keys resource-cache))
             character-count
             :mentions (into #{} (map :xt/id (vals mentions)))
             :reply-to reply-to))


          :meme
          (if-let [{:keys [location xt/id mime-type]}
                   (res-meta/get-resource-by-id xtdb-node (:base-img payload))]
            (let [{:keys [base-img caption]} payload
                  {:keys [lines background foreground font-family font-size caption-size]}
                  caption

                  [width height resource resource-type]
                  (with-open [resource-file (res-map/open resource-map id)]
                    (case mime-type
                      ("image/png" "image/jpeg")
                      (let [image (img/->buffered-image resource-file)
                            {:keys [width height]} (img/get-image-metadata image)]
                        [width height image :image])

                      "image/gif"
                      (let [{:keys [width height] :as gif}
                            (gif/get-gif-from-stream resource-file)]
                        [width height gif :gif])

                      (bad-request! {:error :invalid-resource-type
                                     :resource-type mime-type
                                     :message "The resource type is invalid"})))

                  {:keys [ap-id]} (users/get-user-by-id xtdb-node user-id)

                  hiccup
                  [:meme
                   (into
                    [:caption {:background background
                               :foreground foreground
                               :font-family font-family
                               :font-size font-size}]
                    (for [{:keys [text x y]} lines]
                      [:p {:x x :y y} text]))
                   
                   [:image {:src location}]]
                  
                  [{:keys [mentions character-count]} svg-doc] 
                  (check-sanitization
                   (h/sanitize-meme-hiccough
                    hiccup
                    false
                    (partial get-user-by-name xtdb-node)
                    width height (* caption-size width)))]
              (statuses/create-status
               user-id
               {:svg (svg/doc->string svg-doc)}
               :meme
               :svg
               (join ap-id "./statuses/")
               #{base-img}
               character-count
               :mentions (into #{} (map :xt/id (vals mentions)))
               :reply-to reply-to))
            (bad-request! {:error :no-resource
                           :message "The resource you're trying to use does not exist"})))
        transaction [(statuses/insert-status-tx status)
                     (db-timeline/insert-timeline-doc (db-timeline/create-pub-doc user-id id))]]
    (if async? ; TODO respond with full URI
      (submit-and-accept xtdb-node transaction ap-id)
      (exec-and-ok xtdb-node transaction status))))


(defn repub-post!
  [{:keys [xtdb-node]}
   {{{:keys [status-id]} :body} :parameters
    {:keys [user-id]} :session
    :keys [mushin/async?]}]
  (let [{:keys [ap-id] :as status}
        (or (statuses/living-status? xtdb-node type)
            (bad-request! {:error :status-does-not-exist}))

        tx
        [(db-timeline/insert-timeline-doc (db-timeline/create-pub-doc user-id status-id))]]
    (if async?
      (submit-and-accept xtdb-node tx ap-id)
      (exec-and-ok xtdb-node tx status))))

(defn add-media!
  [{:keys [resource-map]}
   {:keys [params]
    {:keys [user-id]} :session}]
  (ok
   (into
    {}
    (map (fn [[tag {:keys [tempfile]}]]
           (let [upload-type (files/detect-content-type tempfile)]
             [tag 
              (case upload-type
                ("image/jpeg" "image/png")
                (:xt/id
                 (media/create-resource-from-static-image!
                  tempfile
                  upload-type
                  resource-map))

                "image/gif"
                (:xt/id
                 (media/create-resource-from-buffered-image!
                  tempfile
                  upload-type
                  resource-map))

                (bad-request! {:error :invalid-upload-type
                               :upload-type upload-type}))])))
    params)))

(defn get-timeline
  [{:keys [xtdb-node]}
   {{:keys [user-id]} :session}]
  (ok {:timeline (db-timeline/get-user-events xtdb-node user-id)}))


(defn get-likes
  [{:keys [xtdb-node]}
   {{{:keys [valid order-column offset-by n reverse]
      :or {order-column 'created-at
           offset-by    0
           n            20}} :query} :parameters
    {:keys [user-id]}  :session}]
  (xt/q
   xtdb-node
   [(xt/template
     (fn [actor-id]
       (->
        (from :mushin.db/likes
              ~(db/query-bind '[xt/id status-id {:actor-id actor-id
                                                 :xt/valid-from valid-from
                                                 :xt/valid-to vto}]
                              valid nil))
        (order-by {:val ~(case order-column
                           :valid-from 'valid-from
                           :valid-to 'valid-to
                           'created-at)
                   :dir ~(if reverse :asc :desc)
                   :nulls :last})
        (offset ~offset-by)
        (limit ~n)
        (without ~(if valid :__nothing_ever_happens :valid-from)
                 ~(if valid :__nothing_ever_happens :valid-to)))))
    user-id]))

(defn block-user!
  "API handler for blocking a user."
  [{:keys [xtdb-node]}
   {{{:keys [id]} :path} :parameters
    {:keys [user-id]}  :session
    :keys [mushin/async?]}]
  (when (rel/has-relationship? xtdb-node :block user-id id)
    (conflict! {:error :user-already-blocked :message "You have already blocked that user" :user-id id}))
  (if async?
    (db/compose-and-submit-txs! xtdb-node (rel/insert-block-tx user-id id))
    (db/compose-and-execute-txs! xtdb-node (rel/insert-block-tx user-id id)))
  (no-content))

(defn mute-user!
  "API handler for blocking a user."
  [{:keys [xtdb-node]}
   {{{:keys [id]} :path} :parameters
    {:keys [user-id]}  :session
    :keys [mushin/async?]}]
  (when (rel/has-relationship? xtdb-node :block user-id id)
    (conflict! {:error :user-already-muted :message "You have already muted that user" :user-id id}))
  (if async?
    (db/compose-and-submit-txs! xtdb-node (rel/insert-mute-tx user-id id))
    (db/compose-and-execute-txs! xtdb-node (rel/insert-mute-tx user-id id)))
  (no-content))


(defn follow-user!
  "API handler for following a user."
  [{:keys [xtdb-node]}
   {{{:keys [id]} :path} :parameters
    {:keys [user-id]}  :session
    :keys [mushin/async?]}]
  (let [can-follow (rel/follow-tx xtdb-node user-id id)]
    (if (vector? can-follow)
      (do
        (if async?
          (db/compose-and-submit-txs! xtdb-node can-follow)
          (db/compose-and-execute-txs! xtdb-node can-follow))
        (no-content))

      (case can-follow
        :following
        (conflict! {:error :user-already-muted
                    :message "You have already muted that user"
                    :user-id user-id
                    :target-id id})

        :self-follow
        (bad-request! {:error :self-follow
                       :message "You cannot follow yourself"
                       :user-id user-id
                       :target-id id})

        :denied
        (unauthorized! {:error :denied
                        :message "You cannot follow that user"
                        :user-id user-id
                        :target-id id})))))

(defn follows?
  [{:keys [xtdb-node]}
   {{{:keys [id]} :path} :parameters
    {:keys [user-id]}    :session}]
  )

(defn unmute-user!
  [{:keys [xtdb-node]}
   {{{:keys [id]} :path} :parameters
    {:keys [user-id]}  :session
    :keys [mushin/async?]}]
  (if async?
    (xt/submit-tx xtdb-node [(rel/delete-realtion-tx :mute user-id id)])
    (xt/execute-tx xtdb-node [(rel/delete-realtion-tx :mute user-id id)]))
  (no-content))


(defn unblock-user!
  [{:keys [xtdb-node]}
   {{{:keys [id]} :path} :parameters
    {:keys [user-id]}  :session
    :keys [mushin/async?]}]
  (if async?
    (xt/submit-tx xtdb-node [(rel/delete-realtion-tx :block user-id id)])
    (xt/execute-tx xtdb-node [(rel/delete-realtion-tx :block user-id id)]))
  (no-content))

(defn unfollow-user!
  [{:keys [xtdb-node]}
   {{{:keys [id]} :path} :parameters
    {:keys [user-id]}  :session
    :keys [mushin/async?]}]
  (let [unfollow-tx (rel/unfollow-tx xtdb-node user-id id)]
    (if (vector? unfollow-tx)
      (do
        (if async?
          (db/compose-and-submit-txs! xtdb-node unfollow-tx)
          (db/compose-and-execute-txs! xtdb-node unfollow-tx))
        (no-content))
      (bad-request!
       {:error unfollow-tx
        :message (case unfollow-tx
                   :not-following
                   "You cannot unfollow a user you are not following"

                   :self-unfollow
                   "You cannot unfollow yourself")}))))


(defn get-following
  [{:keys [xtdb-node]}
   {{{:keys [order-column offset-by n reverse cursor]
      :or {order-column :created-at
           offset-by    0
           n            20}
      :as q} :query} :parameters
    {:keys [user-id]} :session}]

  (ok (rel/get-related-accounts xtdb-node :follow user-id true q)))

(defn get-followers
  [{:keys [xtdb-node]}
   {{{:keys [order-column offset-by n reverse]
      :or {order-column :created-at
           offset-by    0
           n            20}
      :as q} :query} :parameters
    {:keys [user-id]} :session}]
  (ok (rel/get-related-accounts xtdb-node :follow user-id false q)))

(defn get-blocked-accounts
  [{:keys [xtdb-node]}
   {{{:keys [order-column offset-by n reverse]
      :or {order-column :created-at
           offset-by    0
           n            20}
      :as q} :query} :parameters
    {:keys [user-id]} :session}]
  (ok (rel/get-related-accounts xtdb-node :block user-id true q)))


(defn get-muted-accounts
  [{:keys [xtdb-node]}
   {{{:keys [order-column offset-by n reverse]
      :or {order-column :created-at
           offset-by    0
           n            20}
      :as q} :query} :parameters
    {:keys [user-id]} :session}]
  (ok (rel/get-related-accounts xtdb-node :mute user-id true q)))
