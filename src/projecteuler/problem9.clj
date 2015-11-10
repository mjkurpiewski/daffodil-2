;; A Pythagorean triple is composed of 3 elements, (a, b, c) for which
;; a < b < c && (* a a) + (* b b) = (* c c)
;; We are tasked with finding the Pythagorean triple whose elements
;; a + b + c = 1,000.

;; The basic Pythagorean triple is (3, 4, 5). From this, by means of
;; 3 transformations, T1-T3, we can derive all Pythagorean triples we may
;; desire.

;; We will generate new sets of Pythagorean triples until we find the one
;; that fulfills the required relation. This should not require many
;; transformations.

;; After finding the triple, we return its product.

;; T1 = {:a (+ 2c (- a 2b)),
;;       :b (+ 2c (- 2a b)),
;;       :c (+ 3c (- 2a 2b))}

;; T2 = {:a (+ a 2b 2c),
;;       :b (+ 2a b 2c),
;;       :c (+ 2a 2b 3c)}

;; T3 = {:a (- (+ 2b 2c) a),
;;       :b (- (+ b 2c) 2a),
;;       :c (- (+ 2b 3c) 2a)}

(ns projecteuler.problem9)

(def base-triple {:a 3,
                  :b 4,
                  :c 5})

(defn perfect-triple?
  "Given a Pythagorean triple map, returns true if a < b < c, false
  otherwise. We can assume that any triple map passed in fulfills the
  relation (* a a) + (* b b) = (* c c).

  Input <- triple, a map with keys :a, :b, and :c
  Output -> boolean"
  [{:keys [a, b, c]}]
  (if (< a b c)
    true
    false))

(defn target-value?
  "Given a Pythagorean triple map, returns true if the map is both a
  perfect triple and its a, b, and c values sum to n.

  Input  <- triple, a map with keys :a, :b, and :c,
            n, an integer that fulfills the relation (= (+ a b c) 1000)
  Output -> boolean"
  [m n]
  (let [{:keys [a, b c]} m]
    (if (and (= n (+ a b c))
             (perfect-triple? m))
      true
      false)))

(defn generate-new-triples
  "Given a Pythagorean triple map, returns a nested map with keys T1, T2, and T3.
  This applies the transformations detailed on lines 16 through 26 to the
  input triplet map. We destructure the map into its a, b, and c components
  when the map is passed to this function.

  Input <- triple, a map with keys :a, :b, and :c
  Output -> a map of triple maps with keys :T1, :T2, and :T3"
  [{:keys [a, b, c]}]
  (let [a2 (* 2 a),
        b2 (* 2 b),
        c2 (* 2 c),
        c3 (* 3 c)]
    {:T1 {:a (+ c2 (- a b2)),
          :b (+ c2 (- a2 b)),
          :c (+ c3 (- a2 b2))},
     :T2 {:a (+ a b2 c2),
          :b (+ a2 b c2),
          :c (+ a2 b2 c3)},
     :T3 {:a (- (+ b2 c2) a),
          :b (- (+ b c2) a2),
          :c (- (+ b2 c3) a2)}}))

(defn find-n-perfect-triple
  "Given a value of n for which (= n (+ a b c)) and a, b, and c comprise a
  perfect Pythagorean triple, finds the values of a, b, and c that fulfill
  the previous relation.

  Input <- n, the value to which a, b, and c should sum
  Output -> a map with keys :a, :b, and :c, that is a perfect Pythagorean triple
            and whose values sum to n"
  [n]
  (let [first-generation (generate-new-triples base-triple)]
    (loop [])))
