(ns org.mushin.resources.resource-map)


(defprotocol ResourceMap
  (create! [this name input-stream] "Create a resource referenced by `name` with data in `input-stream`.
If a resource already exists with `name` then this function is a no-op.")
  (open [this name] "Get a stream that references the resource referenced by `name`.")
  (exists? [this name] "Returns true if a resource referenced by `name` exists, false if not.")
  (delete! [this name] "Delete the resource referenced by `name`.  If the resource doesn't exist this operation is a no-op."))
