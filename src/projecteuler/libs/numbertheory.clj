(ns projecteuler.libs.numbertheory
  (:import (java.lang Math)))

(defn gcd
  "Basic recursive solution to finding the greatest common divisor of two numbers using
  Euclid's method.

  Input <- a and b, two integers.
  Output -> A non-zero integer that is the greatest common divisor."
  [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

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

(defn prime?
  "A function that takes an integer, n, and tests it for primality. We immediately
  check whether or not the integer is even, then proceed to perform trial division
  until we reach the square root of n.

  Input <- n, an integer, to be tested for primality.
  Output -> Mixed output, returns the prime if true, false otherwise. (THIS MAY BE POOR FORM)"
  [n]
  (if (even? n)
    false
    (let [square-root (int (Math/sqrt n))]
      (loop [i 3]
        (if (> i square-root)
          n
          (if (zero? (mod n i))
            false
            (recur (+ i 2))))))))


