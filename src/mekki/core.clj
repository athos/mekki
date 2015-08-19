(ns mekki.core
  (:refer-clojure :exclude [compile])
  (:require [clojure.core.match :as m])
  (:import [edu.mit.csail.sdg.alloy4compiler.ast
            Sig Sig$PrimSig Sig$SubsetSig Attr Func Decl Expr ExprConstant]
           [edu.mit.csail.sdg.alloy4 Util]))

(def the-ns-name (ns-name *ns*))

;;
;; Utilities
;;

(defmacro $ [sym]
  `'~(symbol (str 'edu.mit.csail.sdg.alloy4compiler.ast. sym)))

(defn add-tag [x tag]
  (vary-meta x assoc :tag tag))

(defmacro ^:private match [expr & clauses]
  `(m/match ~expr
     ~@(->> (for [[pattern action] (partition 2 clauses)]
              (m/match pattern
                (['quote sym] :seq)
                #_=> [`(:or '~sym '~(symbol (str the-ns-name) (str sym)))
                      action]
                ([(['quote sym] :seq) & rest] :seq)
                #_=> [`([(:or '~sym '~(symbol (str the-ns-name) (str sym)))
                         ~@rest]
                        :seq)
                      action]
                (s :guard seq?) [`(~(vec s) :seq) action]
                (v :guard vector?) [(seq v) action]
                :else [pattern action]))
            (apply concat))))

;;
;; Signature definition
;;

(defmacro defsig [signame & {:keys [extends in]}]
  (let [meta (meta signame)
        attrs (cond-> []
                (:abstract meta) (conj ($ Attr/ABSTRACT))
                (:lone meta) (conj ($ Attr/LONE))
                (:one meta) (conj ($ Attr/ONE))
                (:some meta) (conj ($ Attr/SOME)))]
    `(def ~(add-tag signame ($ Sig))
       ~(if in
          `(Sig$SubsetSig. ~(name signame) ~in (into-array Attr ~attrs))
          `(Sig$PrimSig. ~(name signame) (into-array Attr ~attrs))))))

(declare compile)

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
  (loop [decls decls, ret []]
    (match decls
      [_ :guard empty?] ret
      ((decl-name :guard symbol?) :- decl-type & decls')
      #_=> (let [decl-name (add-tag decl-name ($ Decl))
                 compiled-decl (compile-decl env decl-name decl-type)]
             (recur decls' (conj ret [decl-name compiled-decl])))
      :else (throw (IllegalArgumentException. "malformed decl")))))

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

(defn- compile-seq [env expr]
  (match expr
    ('not expr1) `(.not ~(compile env expr1))
    ('no expr1) `(.no ~(compile env expr1))
    ('one expr1) `(.one ~(compile env expr1))
    ('lone expr1) `(.lone ~(compile env expr1))
    ('some expr1) `(.some ~(compile env expr1))
    ('set expr1) `(.set ~(compile env expr1))
    ('count expr1) `(.cardinarity ~(compile env expr1))
    ('trans expr1) `(.transpose ~(compile env expr1))
    ('* expr1) `(.reflexiveClosure ~(compile env expr1))
    ('or expr1 expr2) `(.or ~(compile env expr1) ~(compile env expr2))
    ('iff expr1 expr2) `(.iff ~(compile env expr1) ~(compile env expr2))
    ('if expr1 expr2) `(.implies ~(compile env expr1) ~(compile env expr2))
    ('if expr1 expr2 expr3)
    #_=> `(.ite ~(compile env expr1) ~(compile env expr2) ~(compile env expr3))
    ('& expr1 expr2) `(.intersect ~(compile env expr1) ~(compile env expr2))
    ('+ expr1 expr2) `(.plus ~(compile env expr1) ~(compile env expr2))
    ('- expr1 expr2) `(.minus ~(compile env expr1) ~(compile env expr2))
    ('++ expr1 expr2) `(.override ~(compile env expr1) ~(compile env expr2))
    ('. expr1 expr2) `(.join ~(compile env expr1) ~(compile env expr2))
    ('-> expr1 expr2) `(.product ~(compile env expr1) ~(compile env expr2))
    ('in expr1 expr2) `(.in ~(compile env expr1) ~(compile env expr2))
    ('= expr1 expr2) `(.equal ~(compile env expr1) ~(compile env expr2))
    ('< expr1 expr2) `(.lt ~(compile env expr1) ~(compile env expr2))
    ('> expr1 expr2) `(.gt ~(compile env expr1) ~(compile env expr2))
    ('<= expr1 expr2) `(.lte ~(compile env expr1) ~(compile env expr2))
    ('>= expr1 expr2) `(.gte ~(compile env expr1 ~(compile env expr2)))))

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
