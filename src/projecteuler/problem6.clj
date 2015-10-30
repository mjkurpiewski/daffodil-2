;; The sum of the squares of the first ten natural numbers is,
;; 1^2 + 2^2 + ... + 10^2 = 385

;; The square of the sum of the first ten natural numbers is,
;; (1 + 2 + ... + 10)^2 = 55^2 = 3025

;; Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025 − 385 = 2640.
;; Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.

(ns projecteuler.problem6)

(defn find-difference
  "Finds the difference between the sum of the squares of the first n natural
  numbers and the square of the sum of the first n natural numbers.

  Input <- n, an integer, representing the upper boundary of number range
  Output -> the difference between the sum of squares and the square of the
            sum, an integer"
  [n]
  (let [active-range (range 1 (+ n 1))]
    (long (- (Math/pow (reduce + active-range) 2)
             (reduce + (map (fn [e]
                              (Math/pow e 2))
                            active-range))))))
