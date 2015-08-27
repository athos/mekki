(ns mekki.util
  (:import [edu.mit.csail.sdg.alloy4compiler.ast Sig]))

(defn ns-sigs [ns]
  (for [[_ v] (ns-publics (the-ns ns))
        :when (= (:tag (meta v)) Sig)]
    (deref v)))
