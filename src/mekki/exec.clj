(ns mekki.exec
  (:require [mekki
             [lang :as lang]])
  (:import [edu.mit.csail.sdg.alloy4compiler.ast Command]
           [edu.mit.csail.sdg.alloy4compiler.translator
            A4Options A4Options$SatSolver TranslateAlloyToKodkod]
           [edu.mit.csail.sdg.alloy4 A4Reporter]))

(defn- execute [e sigs check?]
  (let [cmd (Command. check? 3 3 3 e)
        opts (A4Options.)]
    (TranslateAlloyToKodkod/execute_command A4Reporter/NOP sigs cmd opts)))

(defn- collect-from-ns [pred ns]
  (for [[_ v] (ns-interns (the-ns ns))
        :when (pred ns)]
    (deref v)))

(defn ns-sigs [ns]
  (collect-from-ns lang/sig? ns))

(defn run-fn [e & {:keys [ns sigs] :or {ns *ns*}}]
  (execute e (or sigs (ns-sigs ns)) false))

(defmacro run [e & opts]
  `(let [opts# (merge {:ns (the-ns '~(symbol (str *ns*)))}
                      (array-map ~@opts))]
     (apply run-fn (lang/expr ~e) (apply concat opts#))))

(defn check-fn [e & {:keys [ns sigs] :or {ns *ns*}}]
  (execute e (or sigs (ns-sigs ns)) true))

(defmacro check [e & opts]
  `(let [opts# (merge {:ns (the-ns '~(symbol (str *ns*)))}
                      (array-map ~@opts))]
     (apply check-fn (lang/expr ~e) (apply concat opts#))))
