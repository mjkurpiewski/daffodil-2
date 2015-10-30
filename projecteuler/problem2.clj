(ns projecteuler.problem2)
;; By considering the terms in the Fibonacci sequence whose values do not exceed four million,
;; find the sum of the even-valued terms.

;; Plan of attack: Function to generate Fibonacci numbers, filter by even?, reduce by +.

(defn fibonacci-generator
  "The following function will generate a lazy sequence of Fibonacci numbers up
  until the most recent term generated in the sequence is greater than the input
  value, n.

  For instance: (fibonacci-generator 4000000) will create a sequence of Fibonacci
  numbers, stopping when the most recently generated Fibonacci number exceeds the
  boundary established by n.

  Input <- n, an integer or big integer establishing an upper boundary on the sequence.
  Output -> A sequence [vector] of Fibonacci numbers up until that boundary."
  [n]
  (loop [fib1 0,
         fib2 1,
         fib-sequence '[0 1]]
    (if (> (+ fib1 fib2) n)
      fib-sequence
      (let [next-fib (+ fib1 fib2)]
        (recur fib2
               next-fib
               (conj fib-sequence next-fib))))))

(defn fibonacci-calculator
  "This function takes a vector of Fibonacci numbers, filters to find only the
  even valued numbers, then returns their sum.

  Input <- n, an integer or big integer that establishes the upper boundary on
           the sequence of Fibonacci numbers.
  Output -> an integer or big integer representing the value of the sum of only
            the even Fibonacci in a given range."
  [n]
  (reduce +
          (filter even?
                  (fibonacci-generator n))))
