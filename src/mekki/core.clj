(ns mekki.core
  (:refer-clojure :exclude [compile])
  (:require [clojure.core.match :as m])
  (:import [edu.mit.csail.sdg.alloy4compiler.ast
            Sig Sig$PrimSig Sig$SubsetSig Attr Func Decl Expr ExprConstant ExprCall]
           [edu.mit.csail.sdg.alloy4 Util]))

(def the-ns-name (ns-name *ns*))

;;
;; Utilities
;;

(defn- qualify [sym]
  (symbol (str the-ns-name) (str sym)))

(defmacro $ [sym]
  `'~(symbol (str 'edu.mit.csail.sdg.alloy4compiler.ast. sym)))

(defn add-tag [x tag]
  (vary-meta x assoc :tag tag))

(defmacro ^:private match [expr & clauses]
  `(m/match ~expr
     ~@(->> (for [[pattern action] (partition 2 clauses)]
              (m/match pattern
                (['quote sym] :seq)
                #_=> [`(:or '~sym '~(qualify sym)) action]
                ([(['quote sym] :seq) & rest] :seq)
                #_=> [`([(:or '~sym '~(qualify sym)) ~@rest] :seq) action]
                (s :guard seq?) [`(~(vec s) :seq) action]
                (v :guard vector?) [(seq v) action]
                :else [pattern action]))
            (apply concat))))

(defn- map-decls [f decls]
  (loop [decls decls, ret []]
    (match decls
      [_ :guard empty?] ret
      ((decl-name :guard symbol?) :- decl-type & decls')
      #_=> (recur decls' (conj ret (f decl-name decl-type)))
      :else (throw (IllegalArgumentException. "malformed decl")))))

;;
;; Signature definition
;;

(declare compile)

(defn- parse-opts [opts]
  (loop [[maybe-keyword maybe-arg & opts' :as opts] opts, ret {}]
    (cond (empty? opts) ret
          (= maybe-keyword :in)
          #_=> (recur opts' (assoc ret :in maybe-arg))
          (= maybe-keyword :extends)
          #_=> (recur opts' (assoc ret :parent maybe-arg))
          (vector? maybe-keyword)
          #_=> (assoc ret :fields maybe-keyword))))

(defmacro defsig [signame & opts]
  (let [meta (meta signame)
        attrs (cond-> []
                (:abstract meta) (conj ($ Attr/ABSTRACT))
                (:lone meta) (conj ($ Attr/LONE))
                (:one meta) (conj ($ Attr/ONE))
                (:some meta) (conj ($ Attr/SOME)))
        {:keys [in parent fields]} (parse-opts opts)]
    `(do (def ~(add-tag signame ($ Sig))
           ~(if in
              `(Sig$SubsetSig. ~(name signame) ~in (into-array Attr ~attrs))
              `(Sig$PrimSig. ~(name signame)
                             ~@(if parent [parent])
                             (into-array Attr ~attrs))))
         ~@(for [[decl-name decl-type] (map-decls list fields)]
             `(.addField ~signame
                         ~(name decl-name)
                         ~(compile #{} decl-type)))
         #'~signame)))

;;
;; Pred/Func definition
;;

(defn- compile-decl [env decl-name decl-type]
  (letfn [(emit-decl [method type]
            `(~method ~(compile env type) ~(name decl-name)))]
    (match decl-type
      ([(maybe-mult :guard symbol?) t] :seq)
      #_=> (if-let [m ('#{one lone some set} maybe-mult)]
             (emit-decl (symbol (str '. m 'Of)) t)
             (emit-decl '.oneOf decl-type))
      :else (emit-decl '.oneOf decl-type))))

(defn- compile-decls [env decls]
  (map-decls (fn [decl-name decl-type]
               [(add-tag decl-name ($ Decl))
                (compile-decl env decl-name decl-type)])
             decls))

(defn reduce-with-and [exprs]
  (reduce (fn [a e] `(.and ~(add-tag a ($ Expr)) ~e)) exprs))

(defn compile-block [env block]
  (if (empty? block)
    ExprConstant/TRUE
    (reduce-with-and (map #(compile env %) block))))

(defn emit-func [funcname params return-type body]
  (let [decls (compile-decls #{} params)
        names (map first decls)]
    `(def ~(add-tag funcname ($ Func))
       (let [~@(apply concat decls)]
         (Func. nil ~(name funcname)
                (Util/asList (into-array Decl ~(mapv first decls)))
                ~return-type
                ~(compile-block (set names) body))))))

(defmacro defpred [predname params & body]
  (emit-func predname params nil body))

(defmacro deffunc [funcname params _ return-type body]
  (emit-func funcname params return-type [body]))

;;
;; Compilation
;;

(defn- compile-integer [n]
  (case n
    0 ($ ExprConstant/ZERO)
    1 ($ ExprConstant/ONE)
    `(ExprConstant/makeNUMBER ~n)))

(defn- compile-symbol [env sym]
  (match sym
    'iden ($ ExprConstant/IDEN)
    'next ($ ExprConstant/NEXT)
    'univ ($ Sig/UNIV)
    'none ($ Sig/NONE)
    'Int ($ Sig/SIGINT)
    [_ :guard #(contains? env %)] `(.get ~sym)
    :else sym))

(def ^:private unary-ops
  '{not not, no no, one one, lone lone, some some, set set
    count cardinality, trans transpose, * reflexiveClosure})

(def ^:private binary-ops
  '{or or, iff iff, if implies, & intersect, + plus, - minus, ++ override,
    . join, -> product, in in, = equal, < lt, > gt, <= lte, >= gte
    ->one any_arrow_one, ->lone any_arrow_lone, ->some any_arrow_some
    ->set preduct, one-> one_arrow_any, one->one one_arrow_one
    one->lone one_arrow_lone, one->some one_arrow_some
    one->set one_arrow_any, lone-> lone_arrow_any, lone->one lone_arrow_one
    lone->lone lone_arrow_lone, lone->some lone_arrow_some
    lone->set lone_arrow_any, some-> some_arrow_any, some->one some_arrow_one
    some->lone some_arrow_lone, some->some some_arrow_some
    some->set some_arrow_any, set-> product, set->one any_arrow_one
    set->lone any_arrow_lone, set->some any_arrow_some, set->set product})

(def ^:private ternary-ops '{if ite})

(def ^:private formulae
  '{no forNo, one forOne, lone forLone, some forSome, all forAll,
    for comprehensionOver})

(def ^:private operators
  (reduce-kv (fn [m k v] (-> m (assoc k v) (assoc (qualify k) v)))
             {}
             (merge unary-ops binary-ops ternary-ops formulae)))

(defn- operator? [expr]
  (contains? operators (first expr)))

(defmacro compile-operator [expr]
  (letfn [(dot [method] (symbol (str '. method)))]
    `(match ~expr
       ~@(mapcat (fn [[op method]]
                   [`('~op (~'decls :guard ~'vector?) ~'& ~'body)
                    `(~'formula '~(dot method) ~'decls ~'body)])
                 formulae)
       ~@(mapcat (fn [[op method]]
                   [`('~op ~'e) `(~'operator '~(dot method) ~'e)])
                 unary-ops)
       ~@(mapcat (fn [[op method]]
                   [`('~op ~'e1 ~'e2) `(~'operator '~(dot method) ~'e1 ~'e2)])
                 binary-ops)
       ~@(mapcat (fn [[op method]]
                   [`('~op ~'e1 ~'e2 ~'e3)
                    `(~'operator '~(dot method) ~'e1 ~'e2 ~'e3)])
                 ternary-ops))))

(defn- compile-seq [env expr]
  (letfn [(operator [method & operands]
            `(~method ~@(map #(compile env %) operands)))
          (formula [method decls body]
            (let [compiled-decls (compile-decls #{} decls)
                  names (map first compiled-decls)]
              `(let [~@(apply concat compiled-decls)]
                 (~method ~(compile-block (into env names) body)
                          ~(first names)
                          (into-array Decl [~@(rest names)])))))]
    (if (operator? expr)
      (compile-operator expr)
      (let [[op & args] expr]
        (if (contains? env op)
          (operator '.join (first args) op)
          (let [v (resolve op)]
            (if (and (var? v) (= (:tag (meta v)) Sig))
              (operator '.join (first args) op)
              `(ExprCall/make nil nil ~op ~(mapv #(compile env %) args) 0))))))))

(defn- compile [env expr]
  (-> (cond (false? expr) ($ ExprConstant/FALSE)
            (true? expr) ($ ExprConstant/TRUE)
            (integer? expr) (compile-integer expr)
            (symbol? expr) (compile-symbol env expr)
            (seq? expr) (compile-seq env expr))
      (add-tag ($ Expr))))

(comment

  (defsig B)
  (defsig ^:abstract A
    [f :- (lone->lone B B)
     g :- B])

  (defsig ^:one A1 :extends A)
  (defsig ^:one A2 :extends A)

  (fact
    (all [this :- A]
      (in (.f this) (lone->lone B B))
      (in (.g this) B)))

  (defpred some-g []
    (some g))

  (defpred at-most-3 [x :- univ, y :- univ]
    (>= (count (+ x y)) 3))

  (run (and (some A) (at-most-3 B B)))

)
