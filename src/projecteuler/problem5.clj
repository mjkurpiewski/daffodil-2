(ns projecteuler.problem5
  (:use [clojure.math.numeric-tower :as math :only (expt)])
  (:use [projecteuler.libs.numbertheory :as nt]))

;; 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
;; What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

(defn trivial-prime-factorize
  "Takes as input a number and a factor base and will return a prime factorization
  over that base.

  Input <- n, an integer, and factor-base, a vector of integers
  Output -> a vector of integers corresponding to the prime factorization
            of n, or a vector containing n if n is prime"
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
  "For some values of m and n, constituting an inclusive range of numbers, and
  a factor-base composed of a vector of integers, returns a sequence of integer
  vectors for the prime factorizations of the numbers or the number itself, if
  prime.

  Input <- m and n, integers constituting an inclusive range, and factor-base,
           a vector of integers
  Output -> A sequence of integer vectors"
  [m n factor-base]
  (for [x (range m (+ n 1))]
    (trivial-prime-factorize x factor-base)))

(defn generate-basis
  "For a value n, generates a full factor base, consisting of a vector of primes
  up to and including n, as well a trial base half the size of n. Both of these
  integer vectors are returned in a map, keyed by :factor-base or :trial-base.

  Input <- n, an integer, which establishes an upper boundary on the elements of
           the factor base
  Output -> a map, with keys :factor-base and :trial-base, which return values of
            of integer vectors"
  [n]
  (let [factor-base (nt/generate-primes-to n),
        trial-size (Math/ceil (/ (count factor-base) 2))
        trial-base (vec (take trial-size factor-base))]
    {:factor-base factor-base,
     :trial-base trial-base}))

(defn filter-out-primes
  "Any vectors smaller than 1 are either prime or the empty vector, so we drop
  them and know that they occur once in our final calculation.

  Input <- a sequence of integer vectors
  Output -> a sequence of integer vectors with length greater than 1"
  [factorizations]
  (filter (fn [x]
            (> (count x) 1))
          factorizations))

(defn find-maximum-exponent
  "Given an integer value, base, and a sequence of maps that are k/v pairs of
  base/# of times it appears in a prime factorization, returns the largest
  time it appeared, the largest exponent of the base across all prime factorizations.

  Input <- a sequence of maps of integer frequences and an integer base power to
  search for the maximum of
  Output -> an integer, the maximum exponent value for the given base"
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
  "Given our map of exponent frequencies and our factor base, we obtain a vector
  of exponents in which each exponent value is the maximum that occurs across
  all of our prime factorizations.

  Input <- frequencies, a sequence of exponent frequency maps and
           factor-base, a vector of integers
  Output -> a vector of the maximum exponent values across the
            factor base"
  [frequencies factor-base]
  (map (partial find-maximum-exponent frequencies) factor-base))

(defn power-map
  "Given a vector of bases and a vector of exponents, raises each element of
  factor-base to its corresponding value in exponents. Returns a sequence of
  integer values that result from the exponentiation.

  Input <- exponents, a vector of integer values and
           factor-base, a vector of integer values
  Output -> a sequence of integer values"
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
