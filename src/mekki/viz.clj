(ns mekki.viz
  (:require [mekki
             [lang :as lang]
             [exec :as exec]])
  (:import [edu.mit.csail.sdg.alloy4compiler.translator A4Solution A4SolutionReader]
           [edu.mit.csail.sdg.alloy4viz VizGUI]
           [edu.mit.csail.sdg.alloy4 Computer XMLNode]
           [java.io File]))

(defn- make-enumerator [sol viz sigs]
  (let [sol (volatile! sol)]
    (reify Computer
      (compute [this filename]
        (let [^A4Solution sol' @sol]
          (when (.satisfiable sol')
            (let [next (.next sol')]
              (.writeXML next filename)
              (.loadXML @viz filename true)
              (vreset! sol next))
            filename))))))

(defn show [^A4Solution sol & {:keys [ns sigs] :or {ns *ns*}}]
  (let [sigs (or sigs (exec/ns-sigs ns))
        ^File temp (File/createTempFile "mekki_solution" ".xml")
        filename (.getPath temp)]
    (.writeXML sol filename)
    (let [viz (volatile! nil)
          enumerator (make-enumerator sol viz sigs)]
      (vreset! viz (VizGUI. false filename nil enumerator nil)))))

(defn run-fn [e & {:keys [ns sigs] :or {ns *ns*}}]
  (show (exec/run-fn e :ns ns :sigs sigs) :ns ns :sigs sigs))

(defmacro run [e & opts]
  `(let [opts# (merge {:ns (the-ns '~(symbol (str *ns*)))}
                      (array-map ~@opts))]
     (apply run-fn (lang/expr ~e) (apply concat opts#))))

(defn check-fn [e & {:keys [ns sigs] :or {ns *ns*}}]
  (show (exec/check-fn e :ns ns :sigs sigs) :ns ns :sigs sigs))

(defmacro check [e & opts]
  `(let [opts# (merge {:ns (the-ns '~(symbol (str *ns*)))}
                      (array-map ~@opts))]
     (apply check-fn (lang/expr ~e) (apply concat opts#))))
