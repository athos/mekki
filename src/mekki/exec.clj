(ns mekki.exec
  (:require [mekki
             [lang :as lang]])
  (:import [edu.mit.csail.sdg.alloy4compiler.ast
            Command Expr ExprVar Type Func ExprList ExprList$Op]
           [edu.mit.csail.sdg.alloy4compiler.translator
            A4Options A4Options$SatSolver TranslateAlloyToKodkod]
           [edu.mit.csail.sdg.alloy4compiler.parser CompModule]
           [edu.mit.csail.sdg.alloy4 A4Reporter Pos ConstList]
           [java.util List]
           [java.lang.reflect Constructor Method]))

(defn- private-method [^Class class method-name & arg-classes]
  (let [args (into-array Class arg-classes)]
    (doto (.getDeclaredMethod class (str method-name) args)
      (.setAccessible true))))

(defn- apply-method [^Method method obj & args]
  (.invoke method obj (object-array args)))

(def ^:private ^Method add-func-method
  (private-method CompModule "addFunc" Pos Pos String Expr List Expr Expr))

(defn- add-func! [mod ^Func func]
  (let [label (.label func)
        decls (.decls func)
        ret-type (.returnDecl func)
        body (.getBody func)]
    (apply-method add-func-method mod nil nil label nil decls ret-type body)))

(defn- make-dummy-module [funcs]
  (let [ctor (doto ^Constructor (first (.getDeclaredConstructors CompModule))
               (.setAccessible true))
        ^CompModule mod (.newInstance ctor (object-array [nil "" ""]))]
    (doseq [^Func func funcs]
      (add-func! mod func))
    mod))

(def ^:private ^Method resolve-command-method
  (private-method CompModule "resolveCommand" Command ConstList Expr))

(defn- resolve-command [^CompModule mod cmd facts-expr]
  (apply-method resolve-command-method mod cmd nil facts-expr))

(defn- make-command [e check? facts funcs]
  (resolve-command (make-dummy-module funcs)
                   (Command. check? 3 3 3 e)
                   (ExprList/make nil nil ExprList$Op/AND facts)))

(defn- collect-from-ns [pred ns]
  (for [[_ v] (ns-interns (the-ns ns))
        :when (pred v)]
    (deref v)))

(defn ns-sigs [ns]
  (collect-from-ns lang/sig? ns))

(defn ns-facts [ns]
  (collect-from-ns lang/fact? ns))

(defn ns-preds [ns]
  (collect-from-ns lang/pred? ns))

(defn ns-funcs [ns]
  (collect-from-ns lang/func? ns))

(defn- execute [expr-or-func check? ns sigs facts preds funcs]
  (let [sigs (or sigs (ns-sigs ns))
        facts (or facts (ns-facts ns))
        funcs (concat (or preds (ns-preds ns))
                      (or funcs (ns-funcs ns)))
        expr (if (instance? Func expr-or-func)
               (ExprVar/make nil (.label ^Func expr-or-func))
               expr-or-func)
        cmd (make-command expr check? facts funcs)
        reporter (proxy [A4Reporter] []
                   (debug [msg]
                     (println "debug:" msg))
                   (parse [msg]
                     (println "parse:" msg))
                   (typecheck [msg]
                     (println "typecheck:" msg))
                   (warning [msg]
                     (println "warning:" msg))
                   (scope [msg]
                     (println "scope:" msg))
                   (bound [msg]
                     (println "bound:" msg))
                   (translate [sol b m skol symm]
                     (println "translate:" sol b m skol symm)))
        opts (A4Options.)]
    (TranslateAlloyToKodkod/execute_command reporter sigs cmd opts)))

(defn run-fn [expr-or-func &
              {:keys [ns sigs facts preds funcs] :or {ns *ns*}}]
  (execute expr-or-func false ns sigs facts preds funcs))

(defmacro run [e & opts]
  `(let [opts# (merge {:ns (the-ns '~(symbol (str *ns*)))}
                      (array-map ~@opts))]
     (apply run-fn (lang/expr ~e) (apply concat opts#))))

(defn check-fn [expr-or-func &
                {:keys [ns sigs facts preds funcs] :or {ns *ns*}}]
  (execute expr-or-func true ns sigs facts preds funcs))

(defmacro check [e & opts]
  `(let [opts# (merge {:ns (the-ns '~(symbol (str *ns*)))}
                      (array-map ~@opts))]
     (apply check-fn (lang/expr ~e) (apply concat opts#))))
