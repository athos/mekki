(ns mekki.core
  (:require [clojure.core.match :refer [match]])
  (:import [edu.mit.csail.sdg.alloy4compiler.ast
            Sig Sig$PrimSig Sig$SubsetSig Attr Func Expr ExprConstant]))

(defmacro $ [sym]
  `'~(symbol (str 'edu.mit.csail.sdg.alloy4compiler.ast. sym)))

(defn add-tag [x tag]
  (vary-meta x assoc :tag tag))

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

(defn- compile [expr]
  (-> (match expr
        false ($ ExprConstant/FALSE)
        true ($ ExprConstant/TRUE)
        0 ($ ExprConstant/ZERO)
        1 ($ ExprConstan/ONE)
        (n :guard integer?) `(~($ ExprConstant/makeNUMBER) ~n)
        'iden ($ ExprConstant/IDEN)
        'next ($ ExprConstant/NEXT)
        'univ ($ Sig/UNIV)
        'none ($ Sig/NONE)
        'Int ($ Sig/SIGINT)
        (v :guard symbol?) `(.get ~v))
      (add-tag ($ Expr))))

(defn emit-func [funcname params return-type expr]
  `(def ~(add-tag funcname ($ Func))
     (Func. nil ~(name funcname) ~params ~return-type ~expr)))

(defn reduce-with-and [exprs]
  (reduce (fn [a e] `(.and ~(add-tag a ($ Expr)) ~e)) exprs))

(defmacro defpred [predname params & body]
  (let [body (if (empty? body)
               ExprConstant/TRUE
               (reduce-with-and (map compile body)))]
    (emit-func predname params nil body)))

(defmacro deffunc [funcname params _ return-type body]
  (emit-func funcname params return-type (map compile body)))

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
