(ns org.mushin.schema.ap
  (:require [malli.core :as mallc]
            [malli.generator :as mallg]
            [org.mushin.config :as mushin-c]))

;; TODO set up a schema for URIs and other common things found in AP.


;; TODO this seems like implementing an LL parser, might look into a library
;; for doing that instead of this?
(def as2-object
  [:schema
   {:registry
    {::ASObject
     [:map
      [:context {:optional true} [:or :string [:vector :string]]]
      [:id :string]
      [:type :string]
      [:actor {:optional true} [:ref ::ASObjectOrLink]]
      [:attachment {:optional true} [:vector [:ref ::ASObjectOrLink]]]
      [:attributedTo {:optional true} [:vector [:ref ::ASObjectOrLink]]]]

     ::ASLink
     [:map
      [:href :string]
      [:rel [:vector :string]]]

     ::ASObjectOrLink
     [:or [:ref ::ASLink] [:ref ::ASObject]]
     }}
   ::ASObjectOrLink])


(mallc/validate
 as2-object
 {:id "https://example.com/a"
  :type "Note"
  :actor {
          :id "https://yup.com"
          :type "Uhhhhuhh"
          }
  })
