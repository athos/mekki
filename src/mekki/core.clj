(ns mekki.core)

(comment

  (defsig ^:abstract A [])
  (defsig B [])

  (defsig ^:one A1 [A])
  (defsig ^:one A2 [A])

  (deffields A
    [f :- lone B -> lone B]
    [g :- B])

  (defpred some-g []
    (some g))

  (defpred at-most-3 [x :- Univ, y :- Univ]
    (>= (count (+ x y)) 3))

  (run (and (some A) (at-most-3 B B)))

)
