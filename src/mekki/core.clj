(ns mekki.core
  (:import [edu.mit.csail.sdg.alloy4compiler.ast
            Sig Sig$PrimSig Attr]))

(defmacro defsig [signame parents]
  `(def ~(with-meta signame {:tag Sig})
     (Sig$PrimSig. ~(name signame) (into-array Attr []))))

(comment

  (defsig B)
  (defsig ^:abstract A
    [f :- (lone->lone B B)]
    [g :- B])

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
