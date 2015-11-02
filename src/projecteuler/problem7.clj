;; By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
;; What is the 10,001st prime number?

;; Reference - Rosser, Barkley - Explicit Bounds for Some Functions of Prime Numbers.pdf 

;; n ln n + n(ln ln n - 1) < p(n) < n ln n + n ln ln n

(ns projecteuler.problem7)

(defn locate-nth-prime
  [n]
  (let [component (Math/log n),
        lower-bound (long (Math/floor (+ (* n component)
                                         (* n (- (Math/log component) 1))))),
        upper-bound (long (Math/ceil (+ (* n component)
                                        (* n (Math/log component)))))]
    (println "n: " n)
    (println "Lower bound: " lower-bound)
    (println "Upper bound: " upper-bound)

    (filter integer? (map projecteuler.libs.numbertheory/prime? (range lower-bound
                                                                       upper-bound)))
    ))

