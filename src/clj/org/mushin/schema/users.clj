(ns org.mushin.schema.users)

(def user-schema
  [:nickname  [:and [:string {:min 1 :max 32}] [:re #"\w+"]]])
