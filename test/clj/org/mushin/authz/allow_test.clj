(ns org.mushin.authz.allow-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
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

(deftest can-minor-see-nsfw?
  (is (not (authz/do-rules-allow? (:rules minor-role) :view #{:18+})))
  "Minor user should not be able to view 18+ content.")

(deftest can-minor-create-nsfw?
  (is (not (authz/do-rules-allow? (:rules minor-role) :create #{:18+})))
  "Minor user should not be able to create 18+ content.")

(deftest can-minor-see-untagged-post?
  (is (authz/do-rules-allow? (:rules minor-role) :view #{}))
  "Minor user should be able to see untagged post")

(deftest can-minor-create-untagged-post?
  (is (authz/do-rules-allow? (:rules minor-role) :create #{}))
  "Minor user should be able to create untagged post")
