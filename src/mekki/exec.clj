(ns mekki.exec
  (:require [mekki
             [lang :as lang]])
  (:import [edu.mit.csail.sdg.alloy4compiler.ast
            Command Expr ExprList ExprList$Op]
           [edu.mit.csail.sdg.alloy4compiler.translator
            A4Options A4Options$SatSolver TranslateAlloyToKodkod]
           [edu.mit.csail.sdg.alloy4 A4Reporter]))

(defn- make-command [e check? facts]
  (let [formula (cond-> (ExprList/make nil nil ExprList$Op/AND facts)
                  check? (.addArg (.not e))
                  (not check?) (.addArg e))]
    (Command. check? 3 3 3 formula)))

(defn- execute [^Expr e check? sigs facts]
  (let [cmd (make-command e check? facts)
        opts (A4Options.)]
    (TranslateAlloyToKodkod/execute_command A4Reporter/NOP sigs cmd opts)))

(defn- collect-from-ns [pred ns]
  (for [[_ v] (ns-interns (the-ns ns))
        :when (pred v)]
    (deref v)))

(defn ns-sigs [ns]
  (collect-from-ns lang/sig? ns))

(defn ns-facts [ns]
  (collect-from-ns lang/fact? ns))

(defn run-fn [e & {:keys [ns sigs facts] :or {ns *ns*}}]
  (execute e false (or sigs (ns-sigs ns)) (or facts (ns-facts ns))))

(defmacro run [e & opts]
  `(let [opts# (merge {:ns (the-ns '~(symbol (str *ns*)))}
                      (array-map ~@opts))]
     (apply run-fn (lang/expr ~e) (apply concat opts#))))

(defn check-fn [e & {:keys [ns sigs facts] :or {ns *ns*}}]
  (execute e true (or sigs (ns-sigs ns)) (or facts (ns-facts ns))))

(defmacro check [e & opts]
  `(let [opts# (merge {:ns (the-ns '~(symbol (str *ns*)))}
                      (array-map ~@opts))]
     (apply check-fn (lang/expr ~e) (apply concat opts#))))
