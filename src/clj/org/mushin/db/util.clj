;; This code is adapted from biff's xtdb v2 helper code found here:
;; https://github.com/jacobobryant/biff/blob/d71b8c2422978e070838214d594b716bbd30e11d/libs/xtdb2/src/com/biffweb/xtdb.clj

(ns org.mushin.db.util
  (:require [malli.core :as malli]
            [clj-uuid :as uuid]
            [honey.sql.helpers :as h]
            [honey.sql :as sql]
            [xtdb.api :as xt])
  (:import [xtdb.api Xtdb]))



(defn query-bind
  "Create a BindSpec with optional for-valid-time and for-system-time."
  [bindings valid-for system-valid-for]
  (cond-> {:bind bindings}
    valid-for
    (assoc :for-valid-time (let [[type date1 date2] valid-for]
                             (case type
                               :at (xt/template (at ~date1))
                               :from (xt/template (from ~date1))
                               :to (xt/template  (to ~date1))
                               :in (xt/template  (in ~date1 ~date2))
                               :all-time :all-time)))
    system-valid-for
    (assoc :for-system-time (let [[type date1 date2] system-valid-for]
                              (case type
                                :at (xt/template (at ~date1))
                                :from (xt/template (from ~date1))
                                :to (xt/template  (to ~date1))
                                :in (xt/template (in ~date1 ~date2))
                                :all-time :all-time)))))

(def ^:private xtdb-node?
  [:fn #(instance? Xtdb %)])

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

(defn- check-lookup-args [fn-name node table kvs]
  (check-arity (even? (count kvs)) (+ (count kvs) 2) fn-name)
  (check-args node xtdb-node? table :keyword))

(defn lookup-by-id
  ([node table cols id]
   (first (xt/q node (xt/template (-> (from ~table [~@cols {:xt/id ~id}])
                                      (limit 1))))))
  ([node table id]
   (lookup-by-id node table '[*] id)))

(defn lookup-id [node table qs]
  (-> (xt/q node (xt/template (-> (from ~table [xt/id ~qs])
                                  (limit 1))))
      first
      :xt/id))

(defn lookup-ids
  [node table qs]
  (map :xt/id (xt/q node
                    (xt/template (from ~table [xt/id ~qs])))))

(defn lookup-everything [node table]
  (xt/q node (xt/template (from ~table [*]))))

(defn lookup [node table cols qs]
  (xt/q node (xt/template (from ~table [~@cols ~qs]))))

(defn lookup-first
  ([node table cols qs]
   (first (xt/q node (xt/template (-> (from ~table [~@cols ~qs])
                                      (limit 1))))))
  ([node table qs]
   (lookup-first node table '[*] qs)))

(defn lookup-exists-any? [node table qs]
  (boolean (first (xt/q node
                        (xt/template (-> (from ~table [~qs])
                                         (limit 1)))))))

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

(defn record-exists? [node table id]
  (boolean (first (xt/q node (xt/template
                              (-> (from ~table [{:xt/id ~id}])
                                  (limit 1)))))))

(defn delete-where
  "Create a transaction part for a deleting documents based off a XTQL query.

  # Arguments
   - `table`: The table to delete from
   - `query`: An XTQL query that returns rows to delete

  # Return value
  A XTDB transaction vector."
  [table query]
  [:sql (-> (h/delete-from table)
            (h/where
             [:exists
              [:raw (-> [:xtql query]
                        sql/format
                        first)]])
            (sql/format {:inline true})
            first)])

(defn delete-doc
  "Create a transaction part for deleting a document.

  # Arguments
   - `table`: The name of the table to delete from.
   - `doc`: The document to delete.
   - `columns`: A vector of columns used to determine if the row should be deleted.

  # Return value
  An XTDB transaction vector."
  [table doc columns]
  (delete-where table (xt/template
                       (-> (from ~table [~(select-keys doc columns)])
                           (limit 1)))))


(defn upsert-tx
  "Create a transaction for upserting a document into a table.

  # Arguments
   - `table`: The name of the table to insert into.
   - `doc`: The document to submit.
   - `columns`: A vector of columns used to check if the document already exists.

  # Return value
  A vector of XTDB transactions."
  [table doc columns]
  ;; This is kinda ugly but so far as I know this is only way to do this.
  [(delete-doc table doc columns)
   [:put-docs table doc]])

(defn assert-not-exists-tx
  ""
  [table where]
  [:sql (str "ASSERT NOT EXISTS("
             (-> [:xtql (xt/template
                         (-> (from ~table [~where])
                             (limit 1)))]
                 sql/format
                 first) ")")])


(defn insert-unless-exists-tx
  "Create a transaction for inserting document into a table unless that table already contains a duplicate document.
  When transactioned will throw a `xtdb.error.Conflict` if the check fails.

  # Arguments
   - `table`: The name of the table to insert into.
   - `doc`: The document to submit.
   - `columns`: A vector of columns used to check if the document already exists.

  # Return value
  A vector of XTDB transactions."
  [table doc columns]
  ;; This is kinda ugly but so far as I know this is only way to do this.
  [(assert-not-exists-tx table (select-keys doc columns))
   [:put-docs table doc]])

(defn compile-op-dispatch [node op]
  (first op))

(defmulti compile-op #'compile-op-dispatch)

(defmethod compile-op :default
  [_ op]
  [op])

(defmethod compile-op :sql
  [_ op]
  [op])


;; TODO this is bad. We should just check to see if the document exists
;; and reject tx if it's an insert. If it's an update, we'll check against the schema
;; but ignore missing parts of the doc.
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
                        (merge {:xt/id (uuid/v7)} doc)))]
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

(defn compose-txs
  "Combine XTDB transaction vectors into a single transaction.

  Each argument can be a single statement (e.g.
  `[:put-docs :mushin.db/users {:xt/id (random-uuid)}]`) or a vector of statements
  (e.g. [[:put-docs :mushin.db/users {:xt/id (random-uuid)}] [:sql 'DELETE FROM likes']]),
  or nil."
  [& txs]
  (into []
   (comp (map
          (fn [tx]
            (cond
              (nil? tx) tx
              (and (vector? tx) (vector? (first tx))) tx
              (vector? tx) [tx]
              :else nil)))
         (remove nil?)
         cat)
   txs))

(defn compose-and-submit-txs!
  "Combine XTDB transaction vectors  into a single transaction and execute.

  # Arguments
   - `con`: XTDB connection
   - `txs`: Each argument can be a single statement (e.g.
  `[:put-docs :mushin.db/users {:xt/id (random-uuid)}]`) or a vector of statements
  (e.g. [[:put-docs :mushin.db/users {:xt/id (random-uuid)}] [:sql 'DELETE FROM likes']]),
  or nil.

  # Return value
  See `submit-tx`."
  [con txs]
  (submit-tx con (apply compose-txs txs)))

(defn compose-and-execute-txs!
  "Combine XTDB transaction vectors  into a single transaction and execute.

  # Arguments
   - `con`: XTDB connection
   - `txs`: Each argument can be a single statement (e.g.
  `[:put-docs :mushin.db/users {:xt/id (random-uuid)}]`) or a vector of statements
  (e.g. [[:put-docs :mushin.db/users {:xt/id (random-uuid)}] [:sql 'DELETE FROM likes']]),
  or nil.

  # Return value
  See `submit-tx`."
  [con txs]
  (execute-tx con (apply compose-txs txs)))
