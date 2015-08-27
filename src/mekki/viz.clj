(ns mekki.viz
  (:require [mekki
             [lang :as lang]
             [exec :as exec]
             [util :as util]])
  (:import [edu.mit.csail.sdg.alloy4compiler.translator A4Solution]
           [edu.mit.csail.sdg.alloy4viz VizGUI]
           [java.io File]))

(defn show [^A4Solution sol]
  (let [^File temp (File/createTempFile "mekki_solution" ".xml")
        filename (.getPath temp)]
    (.writeXML sol filename)
    (VizGUI. false filename nil)))

(defn run-fn [e & {:keys [ns sigs] :or {ns *ns*}}]
  (show (exec/run-fn e :ns ns :sigs sigs)))

(defmacro run [e & opts]
  `(run-fn (lang/expr ~e) ~@opts))

(defn check-fn [e & {:keys [ns sigs] :or {ns *ns*}}]
  (show (exec/check-fn e :ns ns :sigs sigs)))

(defmacro check [e & opts]
  `(check-fn (lang/expr ~e) ~@opts))
