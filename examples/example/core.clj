(ns example.core
  (:require [mekki [lang :as lang]
                   [exec :as exec]]))

(lang/defsig ^:abstract A)
(lang/defsig B)

(lang/deffields A
  [f :- (lone->lone B B)
   g :- B])

(lang/defsig ^:one A1 :extends A)
(lang/defsig ^:one A2 :extends A)

(lang/defpred some-g []
  (some g))

(lang/defpred at-most-3 [x :- univ, y :- univ]
  (>= (count (+ x y)) 3))

(defn check []
  (exec/run (and (some A) (at-most-3 B B))))
