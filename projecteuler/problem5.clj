(ns projecteuler.problem5
  (:use [clojure.math.numeric-tower :as math :only (expt)])
  (:use [projecteuler.libs.numbertheory :as nt]))

;; 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
;; What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

(defn trivial-prime-factorize
  "Takes as input a number and a factor base and will return a prime factorization
  over that base."
  [n factor-base]
  (loop [to-reduce n,
         factorization '[],
         factors (vec factor-base)]
    (cond
      (= to-reduce 1)
      factorization,

      (nil? (peek factors))
      false,

      :else
      (if (= (mod to-reduce (first factors)) 0)
        (recur (/ to-reduce (first factors))
               (conj factorization (first factors))
               factors)
        (recur to-reduce
               factorization
               (vec (rest factors)))))))

(defn assemble-prime-factorizations
  [m n factor-base]
  (for [x (range m (+ n 1))]
    (trivial-prime-factorize x factor-base)))

(defn generate-basis
  [n]
  (let [factor-base (nt/generate-primes-to n),
        trial-size (Math/ceil (/ (count factor-base) 2))
        trial-base (vec (take trial-size factor-base))]
    {:factor-base factor-base,
     :trial-base trial-base}))

(defn filter-out-primes
  [factorizations]
  (filter (fn [x]
            (> (count x) 1))
          factorizations))

(defn find-maximum-exponent
  [frequencies base]
  (loop [maximum-exponent 0,
         freq-maps frequencies]
    (cond
      (empty? freq-maps) maximum-exponent,

      (>= (get (first freq-maps) base 1) maximum-exponent)
      (recur (get (first freq-maps) base 1)
             (rest freq-maps)),

      :else
      (recur maximum-exponent
             (rest freq-maps)))))

(defn assemble-exponent-vector
  [frequencies factor-base]
  (map (partial find-maximum-exponent frequencies) factor-base))

(defn power-map
  [exponents factor-base]
  (map (partial math/expt) factor-base exponents))

(defn generate-lcm-over-range
  "Given two integer inputs m and n, representing the start and end of the range,
  inclusive, this function will generate the least common multiple of the numbers
  in the specified range."
  [m n]
  (let [basis (generate-basis n),
        prime-factorizations (assemble-prime-factorizations m n (:factor-base basis)),
        composite-frequencies (map frequencies (filter-out-primes prime-factorizations))]

    (reduce * (power-map (assemble-exponent-vector composite-frequencies
                                                   (:factor-base basis))
                         (:factor-base basis)))))
