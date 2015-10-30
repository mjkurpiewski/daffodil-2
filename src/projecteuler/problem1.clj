(ns projecteuler.problem1)
;; Find the sum of a multiples of 3 or 5 less than 1,000.

(defn desired-multiple?
  "Problem specific predicate for testing whether or not some input value n
  is divisible by 3 or 5.
  Input <- n, an integer to be tested
  Output -> boolean"
  [n]
  (cond
    (= (rem n 3) 0) true
    (= (rem n 5) 0) true
    :else false))


(defn multi-sum
  "This function is designed to calculate the sum of all multiples of 3 and 5
  within the range of 1 to n, exclusive n..
  Input <- n, an integer definining the range
  Output -> sum, an integer for the result"
  [n]
  (reduce +
          (filter desired-multiple?
                  (range 1 n))))
