(ns mekki.exec
  (:require [mekki
             [lang :as lang]
             [util :as util]])
  (:import [edu.mit.csail.sdg.alloy4compiler.ast Command]
           [edu.mit.csail.sdg.alloy4compiler.translator
            A4Options A4Options$SatSolver TranslateAlloyToKodkod]
           [edu.mit.csail.sdg.alloy4 A4Reporter]))

(defn- execute [e sigs check?]
  (let [cmd (Command. check? 3 3 3 e)
        opts (A4Options.)]
    (set! (.solver opts) (A4Options$SatSolver/SAT4J))
    (TranslateAlloyToKodkod/execute_command A4Reporter/NOP sigs cmd opts)))

(defn run-fn [e & {:keys [ns sigs] :or {ns *ns*}}]
  (execute e (or sigs (util/ns-sigs ns)) false))

(defmacro run [e & opts]
  `(run-fn (lang/expr ~e) ~@opts))

(defn check-fn [e & {:keys [ns sigs] :or {ns *ns*}}]
  (execute e (or sigs (util/ns-sigs ns)) true))

(defmacro check [e & opts]
  `(check-fn (lang/expr ~e) ~@opts))
