(ns projecteuler.problem3
  (:import (java.lang Math)))

;; The prime factors of 13195 are 5, 7, 13 and 29.
;; What is the largest prime factor of the number 00851475143?

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
  (let [z2-mod-n (long (mod (* z z) n))]
    (loop [factorization '[]
           to-reduce z2-mod-n
           factors (into [] (rseq factor-base))]

      ;; Debug output
      (println "Factorization: " factorization ", to-reduce: " to-reduce ", factors: " factors)
      ;; End debug output

      (cond
        (= to-reduce 1) factorization
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
  (let [factor-base (generate-primes-to (optimal-b-value n))
        b-smooth-factorizations {}]
    ))
