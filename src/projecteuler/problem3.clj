(ns projecteuler.problem3
  (:require [clojure.core.matrix :as m])
  (:import (java.lang Math)))

;; The prime factors of 13195 are 5, 7, 13 and 29.
;; What is the largest prime factor of the number 600851475143?

(defn gcd
  "Basic recursive solution to finding the greatest common divisor of two numbers using
  Euclid's method.

  Input <- a and b, two integers.
  Output -> A non-zero integer that is the greatest common divisor."
  [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn- debug-smooth
  "I put this in a separate function becuase it seems I rely on this a bit. It
  takes as input the whatever the current state of a factorization is, the number
  being reduced by division of prime factors, and the facts that have been removed
  from the target value.

  Input <- factorization [a vector],
           to-reduce, an integer,
           factors [a vector]
  Output -> (Side effects, println)"
  [factorization to-reduce factors]
  (println "Factorization: " factorization,
           ", to-reduce: " to-reduce,
           ", factors:" factors))

(defn even-coefficients?
  "When applied to a vector of binary coefficients, reduces the vector by addition.
  If the sum is 0, true is returned, because an even coefficient is denoted by 0 in our
  vectors of binary coefficients. Therefore, any non-zero result will return false.

  Input <- v, a vector of binary coefficients (even or odd coefficient values)
  Output -> A boolean, true if reduce returns 0, false otherwise."
  [v]
  (if (zero? (reduce + v))
    true
    false))

(defn reduce-to-binary-coefficients
  "This function will take a vector, such as one produced by reduce-to-coefficients,
  and transform it into a vector where if the coefficient is even, it is replaced with
  a 0. If the coefficient is odd, it is replaced with a 1.

  Input <- v, a vector of coefficients for the factorization of some number
  Output -> A vector of the same length of v where each index represents whether
            or not a value is even (0) or odd (1)."
  [v]
  (into []
        (map (fn [i]
                  (if (even? i)
                    0
                    1))
                v)))

(defn reduce-to-coefficients
  "reduce-to-coefficients will take as input a non-false result from b-smooth?
  i.e. a vector of prime numbers in decreasing order. From this input, the function
  will return a smaller vector, proportional to the size of the factor base, which
  contains the coefficients of each prime number in the factor base in ascending
  order with respect to the factor base.

  Example: [7 5 5 3 2 2 2 2] will be reduced to [4 1 2 1]
           For factorizations that lack factors that are in the factor base, nil
           is replaced with 0, so that [7 5 3 2 2] with a factor base of [2 3 5 7 11]
           will return a coefficient vector of [2 1 1 1 0].

  Input <- v, a vector of prime numbers to be operated on.
           factor-base, the vector of prime factors used in finding smooth numbers.
  Output -> A vector consisting of the coefficients of prime numbers, in ascending
            order with respect to the primes."
  [v factor-base]
  (loop [frequency-map (frequencies (rseq v)),
         base factor-base,
         coefficient-vec '[]]
    (if (empty? base)
      coefficient-vec
      (recur frequency-map
             (rest base)
             (conj coefficient-vec (get frequency-map
                                        (first base)
                                        0))))))

(defn b-smooth?
  "A function, that when given some random integer z, the integer n (which we
  wish to factor,) and the appropriately sized factor base for n factor-base,
  will determine whether or not the square of z is B-smooth with respect to the
  integer to be factorized and the factor base.

  Input <- z, an integer, randomly chosen by dixon-factorization,
           n, the integer to be factorized. Participates in the relationship
              (z^2 mod n), the result of which is checked for B-smoothness,
           factor-base, a vector of integers constituting the factor base.
  Output -> Either a vector representing the expanded prime factorization of
            the value of (z^2 mod n) or false if there is no B-smooth
            factorization with respect to the factor base."
  [z n factor-base]
  (let [z2 (Math/pow z 2.0)
        z2-mod-n (long (mod z2 n))]
    (loop [factorization '[]
           to-reduce z2-mod-n
           factors (into [] (rseq factor-base))]

      ;; Debug output
      ;; (debug-smooth factorization to-reduce factors)
      ;; End debug output

      (cond
        (= to-reduce 1) #spy/p (reduce-to-coefficients factorization factor-base)
        (nil? (peek factors)) false
        :else
        (if (= (mod to-reduce (first factors)) 0)
          (recur (conj factorization (first factors))
                 (/ to-reduce (first factors))
                 factors)
          (recur factorization
                 to-reduce
                 (into [] (rest factors))))))))

(defn optimal-b-value
  "Given an integer, n, this function will employ the relation exp(sqrt(log n log log n))
  to come up with an ideal, optimized B-value to define the factor base to be used in
  determining the B-smoothness of a candidate z value.

  Input <- n, an integer, or long, potentially very large.
  Output -> An integer, ceil'd from the floating point result of the calculation"
  [n]
  (Math/ceil (Math/exp (Math/sqrt (* (Math/log10 n)
                                     (Math/log10 (Math/log10 n)))))))

(defn naive-sieve
  "A simple, lazy implementation of a prime sieve, in part cribbed from the official
  Clojure documentation for lazy-seqs. Given that factor bases will generally be quite
  small, the fact that this is a naive, somewhat inefficient implementation should
  not have too much of an effect. Plus, generation of the factor set only occurs
  once per call of dixon-factorization.

  Input <- s, a sequence of integers, usually starting from 2.
  Output -> A lazy sequence of primes, from which we can take what we need,
            usually only up to double-digit prime numbers."
  [s]
  (cons (first s)
        (lazy-seq (naive-sieve (filter (fn [n]
                                         (not= (mod n (first s))
                                               0))
                                       (rest s))))))

(defn generate-primes-to
  "Given an input n, this function will generate a sequence of primes up to
  and potentially including n.

  Input <- n, an integer, potentially prime.
  Output -> A sequence of prime numbers up to and potentially including n."
  [n]
  (loop [factor-base '[]
         prime-generator (naive-sieve (iterate inc 2))]
    (if (> (first prime-generator) n)
      factor-base
      (recur (conj factor-base (first prime-generator))
             (drop 1 prime-generator)))))

(defn random-in-range
  "Generates a random number within the range specified by the arguments to
  random-in-range. These may be very bad random numbers, because I am generating
  them using a rand function that operates on doubles, and then casting the result
  to a long. They look pretty random and reasonable, but...

  Input <- x, an integer or long defining the lower bound of the range (exclusive)
           y, an integer or long defining the upper bound of the range (exclusive)
  Output -> A random long that hopefully falls within the range specified and is
            sufficiently random for our purposes. *crosses fingers*"
  [x y]
  (long (+ x (rand (- y x)))))

(defn generate-b-smooths
  "Given a value of n, will compute an optimal B-value, the associated
  factor base for that B-value and n, followed by the number of B-smooth
  numbers with respect to n that the algorithm needs to compute in order
  to factorize n. The function will then generate enough of these b-smooth
  relations and then return a map in which the keys are the z values used
  to generate the small prime factorizations, mapped to those factorizations.

  Input <- n, the integer to be factorized.
  Ouput -> smooth-factorizations, a map where the keys are the z values and
           the vals are coefficient vectors that satisfy the relation z^2 mod n
           with respect to the factor base generated by the function. This may
           return a map smaller than that of desired-smooth-numbers if we stumble
           upon a relation whose b-smooth factorization consists only of even
           exponents."
  [n]
  (let [factor-base (generate-primes-to (optimal-b-value n)),
        desired-smooth-numbers (int (+ (count factor-base)
                                       (Math/floor (/ (count factor-base)
                                                      3))))]
    (loop [smooth-factorizations {},
           z (random-in-range (Math/ceil (Math/sqrt n)) n)]
      (let [maybe-smooth (b-smooth? z n factor-base)]
        (cond
          (and maybe-smooth (even-coefficients? (reduce-to-binary-coefficients maybe-smooth)))
          (hash-map z [maybe-smooth factor-base])

          (= (count smooth-factorizations) desired-smooth-numbers)
          smooth-factorizations

          maybe-smooth
          (recur (assoc smooth-factorizations z maybe-smooth)
                 (random-in-range (Math/ceil (Math/sqrt n)) n))

          :else
          (recur smooth-factorizations
                 (random-in-range (Math/ceil (Math/sqrt n)) n)))))))

(defn test-for-divisor
  "Given a relation of two squares in the form of a random z value and a
  1-d matrix, the first element of which being the coefficients, the second
  being the factor base, will check for the existence of a non-trivial
  divisor for n i.e. not 1.

  Input -> z, a randomly generated integer, created by generate-b-smooths,
           m, a 1-d matrix where the first element is a coefficient vector
              and the second element is the factor base."
  [z m]
  (let [coefficents (first m)
        factor-base (second m)]
    (println "z: " z ", coefficients: " coefficents ", factor base: " factor-base)
    (reduce * (map (fn [x y]
                     (Math/pow x y))
                   factor-base
                   coefficents))))

(defn dixon-factorization
  "Given a value, n, this function will attempt to locate the prime factors
  of n. This technique employs random sampling of integers between sqrt(n) and
  n and attempts to ascertain whether or not the squares of those random integers
  are B-smooth, where B is parameter for defining the factor base used in
  the algorithm. The B value for a given n is computed using the function
  optimal-b-value, defined above.

  Input <- n, an integer, potentially very large.
  Output -> p, an integer, the largest prime factor of n."
  [n]
  )





