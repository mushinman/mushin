;; This code is adapted from biff's xtdb v2 helper code found here:
;; https://github.com/jacobobryant/biff/blob/d71b8c2422978e070838214d594b716bbd30e11d/libs/xtdb2/src/com/biffweb/xtdb.clj

(ns org.mushin.db.util
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [malli.core :as malli]
            [malli.error :as me]
            [xtdb.api :as xt]
            [xtdb.node :as xt-node])
  (:import [java.time Instant]
           [xtdb.api Xtdb]))

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

(defn compile-op-dispatch [op]
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

(defmethod compile-op :mushin/upsert
  [[_ table on-doc & [{set-doc :set :keys [defaults]}]]]
  (check-args table :keyword
              on-doc map?
              set-doc [:maybe map?]
              defaults [:maybe map?])
  (let [new-doc (merge {:xt/id (random-uuid)} set-doc defaults on-doc)
        _ (check-table-schema table new-doc)
        on-keys (keys on-doc)
        query (xt/template (-> (from ~table [~(bind-template on-keys)])
                               (limit 1)))]
    [[:call :mushin/if-exists [query on-doc]
      (when (not-empty set-doc)
        [[:update {:table table
                   :bind [(bind-template on-keys)]
                   ;; TODO try to pass in set-doc via args? does it even matter?
                   :set set-doc}
          on-doc]])
      [[:put-docs table new-doc]]]]))

(defmethod compile-op :mushin/delete
  [[_ table & ids]]
  (check-args table :keyword
              ids [:sequential {:min 1} any?])
  [(into [:delete {:from table :bind '[{:xt/id $id}]}]
         (for [id ids]
           {:id id}))])

(defmethod compile-op :mushin/update
  [[_ table {set-doc :set :keys [where]}]]
  (check-args table :keyword
              set-doc map?
              where map?)
  [[:update {:table table
             :bind [(bind-template (keys where))]
             :set (bind-template (keys set-doc))}
    (merge set-doc where)]])

(defmethod compile-op :put-docs
  [[_ table-or-opts & docs :as op]]
  (let [table (if (map? table-or-opts)
                (:into table-or-opts)
                table-or-opts)]
    (doseq [doc docs]
      (check-table-schema table doc))
    [op]))

(defmethod compile-op :update
  [[_ {set-value :set :keys [table]} & arg-rows :as op]]
  (doseq [[attr value] set-value
          :let [schema (-> (malli/schema table)
                           malli/ast
                           :keys
                           attr
                           :value
                           malli/from-ast)]
          args (or arg-rows [{}])
          :let [value (walk/postwalk
                       (fn [x]
                         (let [kw (delay (keyword (subs (str x) 1)))]
                           (if (and (symbol? x)
                                    (str/starts-with? (str x) "$")
                                    (contains? args @kw))
                             (@kw args)
                             x)))
                       value)]]
    (check-attr-schema attr schema value))
  [op])

(defn compile-tx [local-tx]
  (reduce (fn [tx op]
            (into tx (compile-op op)))
          []
          local-tx))

(defn submit-tx [node local-tx]
  (xt/submit-tx node (compile-tx local-tx)))

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

