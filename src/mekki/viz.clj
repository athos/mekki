(ns mekki.viz
  (:require [mekki
             [lang :as lang]
             [exec :as exec]])
  (:import [edu.mit.csail.sdg.alloy4compiler.translator A4Solution A4SolutionReader]
           [edu.mit.csail.sdg.alloy4viz VizGUI]
           [edu.mit.csail.sdg.alloy4 Computer XMLNode]
           [java.io File]))

(defn- make-enumerator [sigs]
  (let [sol (volatile! nil)]
    (reify Computer
      (compute [this filename]
        (when (= @sol :done)
          (when-not @sol
            (let [xml (XMLNode. (File. filename))]
              (vreset! sol (A4SolutionReader/read sigs xml))))
          (let [^A4Solution sol' @sol]
            (when (.satisfiable sol')
              (.writeXML sol' filename)
              (vreset! sol (.next sol'))
              filename)))))))

(defn show [^A4Solution sol & {:keys [ns sigs] :or {ns *ns*}}]
  (let [sigs (or sigs (exec/ns-sigs ns))
        ^File temp (File/createTempFile "mekki_solution" ".xml")
        filename (.getPath temp)]
    (.writeXML sol filename)
    (VizGUI. false filename nil (make-enumerator sigs) nil)))

(defn run-fn [e & {:keys [ns sigs] :or {ns *ns*}}]
  (show (exec/run-fn e :ns ns :sigs sigs) :ns ns :sigs sigs))

(defmacro run [e & opts]
  `(run-fn (lang/expr ~e) ~@opts))

(defn check-fn [e & {:keys [ns sigs] :or {ns *ns*}}]
  (show (exec/check-fn e :ns ns :sigs sigs) :ns ns :sigs sigs))

(defmacro check [e & opts]
  `(check-fn (lang/expr ~e) ~@opts))
