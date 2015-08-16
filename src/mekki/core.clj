(ns mekki.core
  (:require [clojure.core.match :refer [match]])
  (:import [edu.mit.csail.sdg.alloy4compiler.ast
            Sig Sig$PrimSig Sig$SubsetSig Attr Func Decl Expr ExprConstant]
           [edu.mit.csail.sdg.alloy4 Util]))

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

(declare compile)

(defn- compile-decl [decl-name decl-type]
  (letfn [(emit-decl [method type]
            `(~method ~(compile type) ~(name decl-name)))]
    (match decl-type
      ([(maybe-mult :guard symbol?) t] :seq)
      #_=> (if-let [m ('#{one lone some set} maybe-mult)]
             (emit-decl (symbol (str '. m 'Of)) t)
             (emit-decl '.oneOf decl-type))
      :else (emit-decl '.oneOf decl-type))))

(defn- compile-decls [decls]
  (loop [decls decls, ret []]
    (match decls
      ([] :seq) ret
      ([(decl-name :guard symbol?) :- decl-type & decls'] :seq)
      #_=> (let [decl-name (add-tag decl-name ($ Decl))
                 compiled-decl (compile-decl decl-name decl-type)]
             (recur decls' (conj ret [decl-name compiled-decl])))
      :else (throw (IllegalArgumentException. "malformed decl")))))

(defn emit-func [funcname params return-type expr]
  (let [decls (compile-decls params)]
    `(def ~(add-tag funcname ($ Func))
       (let [~@(apply concat decls)]
         (Func. nil ~(name funcname)
                (Util/asList (into-array Decl ~(mapv first decls)))
                ~return-type ~expr)))))

(defn reduce-with-and [exprs]
  (reduce (fn [a e] `(.and ~(add-tag a ($ Expr)) ~e)) exprs))

(defmacro defpred [predname params & body]
  (let [body (if (empty? body)
               ExprConstant/TRUE
               (reduce-with-and (map compile body)))]
    (emit-func predname params nil body)))

(defmacro deffunc [funcname params _ return-type body]
  (emit-func funcname params return-type (map compile body)))

;;
;; Compilation
;;

(defn- compile-integer [n]
  (case n
    0 ($ ExprConstant/ZERO)
    1 ($ ExprConstant/ONE)
    `(ExprConstant/makeNUMBER ~n)))

(defn- compile-symbol [sym]
  (match sym
    'iden ($ ExprConstant/IDEN)
    'next ($ ExprConstant/NEXT)
    'univ ($ Sig/UNIV)
    'none ($ Sig/NONE)
    'Int ($ Sig/SIGINT)
    :else `(.get ~sym)))

(defn- compile [expr]
  (-> (cond (false? expr) ($ ExprConstant/FALSE)
            (true? expr) ($ ExprConstant/TRUE)
            (integer? expr) (compile-integer expr)
            (symbol? expr) (compile-symbol expr))
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
