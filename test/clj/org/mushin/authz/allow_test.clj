(ns org.mushin.authz.allow-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [org.mushin.db.users :as user]
            [org.mushin.db.authorization :as authz]))

(def minor-role "A role representing a person under the age of 18."
  (authz/create-role-doc
   "minor"
   [{:action :create
     :effect :allow
     :tags #{}
     :except-tags #{}
     :match :any}
    {:action :view
     :effect :deny
     :tags #{:18+}
     :except-tags #{}
     :match :any}]))

(def noah "Representative of a moderator."
  (-> (user/create-user "noah" "1234" "noah@example.com")
      (assoc :subject (assoc authz/default-subject-doc :capabilities
                             #{:user.profile:view :user.profile:delete :user.profile:create :user.profile:update}))))

(def kyle "Representative of a regular user on the site."
  (user/create-user "kyle" "Lingua Latina approbator" "kyle@example.com"))


(deftest can-minor-see-nsfw?
  (is (not (authz/do-rules-allow? (:rules minor-role) :view #{:18+})))
  "Minor user should not be able to view 18+ content.")

(deftest can-minor-create-nsfw?
  (is (not (authz/do-rules-allow? (:rules minor-role) :create #{:18+})))
  "Minor user should not be able to create 18+ content.")

(deftest can-mod-view-himself?
  (is (boolean (authz/allowed? noah :view :user noah))
      "Mod should be able to see regular user."))

(deftest can-mod-view-user?
  (is (boolean (authz/allowed? noah :view :user kyle))
      "Mod should be able to see regular user."))
