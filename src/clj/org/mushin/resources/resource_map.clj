(ns org.mushin.resources.resource-map)


(defprotocol ResourceMap
  (create! [this name resource-data] "Create a resource referenced by `name` with data in `resource-data`.
If a resource already exists with `name` then this function is a no-op

# Return
Returns a URI to the resource.
`resource-data` can be either a path object, a string representing a path, a java InputStream, or a java File object.")
  (open [this name] "Get a stream that references the resource referenced by `name`.")
  (metadata [this name] "Get the metadata document for the resource, or `nil` if none exists.")
  (exists? [this name] "Returns true if a resource referenced by `name` exists, false if not.")
  (delete! [this name] "Delete the resource referenced by `name`.  If the resource doesn't exist this operation is a no-op.")
  (to-uri [this name] "Create a URI for the resource referenced by `name`."))
