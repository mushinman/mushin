;; This code is adapted from biff's xtdb v2 helper code found here:
;; https://github.com/jacobobryant/biff/blob/d71b8c2422978e070838214d594b716bbd30e11d/libs/xtdb2/src/com/biffweb/xtdb.clj

(ns org.mushin.db.util
  (:require [clojure.string :as str]
            [malli.core :as malli]
            [xtdb.api :as xt]
            [org.mushin.utils :refer [concat-kw]])
  (:import [xtdb.api Xtdb]))

(defn- fail [& args]
  (throw (ex-info "Unsupported operation." {})))

(def mushin-tx-fns
  '{:mushin/if-exists (fn [[query query-args] true-branch false-branch]
                      (if (not-empty (q query {:args query-args}))
                        true-branch
                        false-branch))})

(def ^:private xtdb-node?
  [:fn #(instance? Xtdb %)])

(defn check-attr-schema [attr schema value]
  (when-not (malli/validate schema value)
    (throw (ex-info "Value doesn't match attribute schema"
                    {:attr attr
                     :schema schema
                     :value value
                     :errors (:errors (malli/explain schema value))}))))

(defn check-table-schema [table document]
  (when-not (malli/validate table document)
    (throw (ex-info "Document doesn't match table schema"
                    {:table table
                     :document document
                     :errors (:errors (malli/explain table document))}))))

(defn -malli-wrap [{:keys [value message] :as error}]
  (str "Invalid argument `" (pr-str value) "`: " message))

(defn check-args* [& arg-maps]
  (doseq [{:keys [value schema quoted-schema]} arg-maps
          :when (not (malli/validate schema value))]
    (throw (ex-info (str "Invalid argument: "
                         (pr-str value) " doesn't satisfy schema `"
                         (pr-str quoted-schema) "`")
                    {:argument value
                     :schema quoted-schema}))))

(defmacro check-args [& args]
  (when-not (even? (count args))
    (throw (clojure.lang.ArityException. (+ (count args) 2) "check-args")))
  `(check-args* ~@(for [[value schema] (partition 2 args)]
                    {:value value
                     :schema schema
                     :quoted-schema (list 'quote schema)})))

(defmacro check-arity [valid n-args fn-name]
  `(when-not ~valid
     (throw (clojure.lang.ArityException. ~n-args ~fn-name))))

(defn compile-op-dispatch [node op]
  (first op))

(defmulti compile-op #'compile-op-dispatch)

(defmethod compile-op :default
  [[:as op]]
  [op])

(defn- bind-template [ks]
  (into {}
        (map (fn [k]
               [k (symbol (str/replace (str k) #"^:" "\\$"))]))
        ks))


(defn lookup-by-id [node table id]
  (let [query (xt/template
               (-> (from ~table [* {:xt/id ~id}])
                   (limit 1)))]
    (first (xt/q node query))))

(defmethod compile-op :patch-docs
  [node [_ table-or-opts & docs :as op]]
  ;; If the patch operation is an UPDATE: we can be sure that the document
  ;; already in the DB is of the correct format.
  ;; If the patch is an INSERT: the document will be incorrect if it has any
  ;; missiing fields. So we combine the documents and then submit.
  (let [table (if (map? table-or-opts)
                (:into table-or-opts)
                table-or-opts)]
    (doseq [doc docs]
      (let [cur-doc (lookup-by-id node table (:xt/id doc))
            new-doc (if cur-doc
                      (merge cur-doc doc)
                      (if (:xt/id doc)
                        doc
                        (merge {:xt/id (random-uuid)} doc)))]
        (check-table-schema table new-doc)))
    [op]))

(defmethod compile-op :put-docs
  [_ [_ table-or-opts & docs :as op]]
  (let [table (if (map? table-or-opts)
                (:into table-or-opts)
                table-or-opts)]
    (doseq [doc docs]
      (check-table-schema table doc))
    [op]))

(defn compile-tx [node local-tx]
  (reduce (fn [tx op]
            (into tx (compile-op node op)))
          []
          local-tx))

(defn submit-tx [node local-tx]
  (xt/submit-tx node (compile-tx node local-tx)))

(defn execute-tx [node local-tx]
  (xt/execute-tx node (compile-tx node local-tx)))

(defn- check-lookup-args [fn-name node table kvs]
  (check-arity (even? (count kvs)) (+ (count kvs) 2) fn-name)
  (check-args node xtdb-node? table :keyword))

(defn lookup-id [node table & kvs]
  (check-lookup-args "lookup-id" node table kvs)
  (let [args (apply hash-map kvs)
        query (xt/template
               (-> (from ~table [xt/id ~(bind-template (keys args))])
                   (limit 1)))]
    (-> (xt/q node query {:args args}) first :xt/id)))

(defn lookup-id-all [node table & kvs]
  (check-lookup-args "lookup-id-all" node table kvs)
  (let [args (apply hash-map kvs)
        query (xt/template (from ~table [xt/id ~(bind-template (keys args))]))]
    (mapv :xt/id (xt/q node query {:args args}))))

(defn lookup [node table & kvs]
  (check-lookup-args "lookup" node table kvs)
  (let [args (apply hash-map kvs)
        query (xt/template
               (-> (from ~table [* ~(bind-template (keys args))])
                   (limit 1)))]
    (first (xt/q node query {:args args}))))

(defn lookup-all [node table & kvs]
  (check-lookup-args "lookup-all" node table kvs)
  (let [args (apply hash-map kvs)
        query (xt/template (from ~table [* ~(bind-template (keys args))]))]
    (xt/q node query {:args args})))
