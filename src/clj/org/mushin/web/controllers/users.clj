(ns org.mushin.web.controllers.users
  (:require [ring.util.http-response :refer [created ok]]
            [org.mushin.db.users :as db]))

(defn user-get-by-uuid
  [{:keys [xtdb-node]}
   {{{:keys [requested-user-id]} :body} :parameters {:keys [user-id]} :session}]
  (db/get-user-by-id xtdb-node requested-user-id))
