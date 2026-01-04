(ns org.mushin.web.utils)

(def id-schema
  "A schema for a map with just a key called `:id` and a `:uuid` value.
  Useful for path params."
  [:map [:id :uuid]])
