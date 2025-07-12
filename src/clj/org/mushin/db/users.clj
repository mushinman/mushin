(ns org.mushin.db.users)

(def schema
  {::short-string [:string {:min 1 :max 256}]
   ::long-string  [:string {:min 1 :max 4096}]

   :user [:map {:closed true}
          [:xt/id     :uuid]
          [:email     [:and ::short-string [:re #".+@.+"]]]
          [:joined-at :time/instant]
          [:last-logged-in-at :time/instant]]})
