;; This exercise began somewhat innocuously, given that finding an answer to the
;; solution is not particularly difficult if you're interested in getting the
;; answer rather than using it as a tool to improve your understanding of CS
;; and mathematics. As a result, I decided to approach solving the problem by
;; implementing Dixon's Factorization Method.
;; In the process of writing this method, I discovered, and was able to take
;; steps to improve, my deficiencies in linear algebra and familiarity with
;; more data-oriented Clojure libraries. So while this program has taken some
;; time, and I can, playing with what I have in the REPL, coax an answer from
;; it, it still is incomplete as I've yet to find a computational method for
;; efficiently utilizing the matrix of exponents to discern a congruence of
;; squares candidate to test for whether or not it contributes a non-trivial
;; factor to our factorization of some number n. Insights into LU-decomposition,
;; whose functionality is provided by Incanter, has led me down a promising path
;; to finding a good solution in the very near future! Stay tuned. :)
;; -pkurpiewski 10/16/2015

(ns projecteuler.problem3
  (:require [clojure.set :as s])
  (:require [clojure.core.matrix :as m])
  (:require [incanter.core :refer :all :as incanter])
  (:use [projecteuler.libs.numbertheory :as nt])
  (:import (java.lang Math)))

;; The prime factors of 13195 are 5, 7, 13 and 29.
;; What is the largest prime factor of the number 600851475143?

(def the-target 600851475143)

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

(defn even-exponents?
  "When applied to a vector of binary exponents, reduces the vector by addition.
  If the sum is 0, true is returned, because an even exponent is denoted by 0 in our
  vectors of binary exponents. Therefore, any non-zero result will return false.

  Input <- v, a vector of binary exponents (even or odd exponent values)

  Output -> A boolean, true if reduce returns 0, false otherwise."
  [v]
  (if (zero? (reduce + v))
    true
    false))

(defn reduce-to-binary-exponents
  "This function will take a vector, such as one produced by reduce-to-exponents,
  and transform it into a vector where if the exponent is even, it is replaced with
  a 0. If the exponent is odd, it is replaced with a 1.

  Input <- v, a vector of exponents for the factorization of some number
          with respect to the factor base.

  Output -> A vector of the same length of v where each index represents whether
            or not a value is even (0) or odd (1)."
  [v]
  (into []
        (map (fn [i]
               (if (even? i)
                 0
                 1))
             v)))

(defn reduce-to-exponents
  "reduce-to-exponents will take as input a non-false result from b-smooth?
  i.e. a vector of prime numbers in decreasing order. From this input, the function
  will return a smaller vector, proportional to the size of the factor base, which
  contains the exponents of each prime number in the factor base in ascending
  order with respect to the factor base.

  Example: [7 5 5 3 2 2 2 2] will be reduced to [4 1 2 1]
           For factorizations that lack factors that are in the factor base, nil
           is replaced with 0, so that [7 5 3 2 2] with a factor base of [2 3 5 7 11]
           will return a exponent vector of [2 1 1 1 0].

  Input <- v, a vector of prime numbers to be operated on.
           factor-base, the vector of prime factors used in finding smooth numbers.

  Output -> A vector consisting of the exponents of prime numbers, in ascending
            order with respect to the primes."
  [v factor-base]
  (loop [frequency-map (frequencies (rseq v)),
         base factor-base,
         exponent-vec '[]]
    (if (empty? base)
      exponent-vec
      (recur frequency-map
             (rest base)
             (conj exponent-vec (get frequency-map
                                        (first base)
                                        0))))))

(defn b-smooth?
  "A function, that when given some random integer z, the integer n (which we
  wish to factor,) and the appropriately sized factor base for n factor-base,
  will determine whether or not the square of z is B-smooth with respect to the
  integer to be factorized and the factor base.

  Input <- z, an integer, randomly chosen by generate-b-smooths,
           n, the integer to be factorized. Participates in the relationship
              (z^2 mod n), the result of which is checked for B-smoothness,
           factor-base, a vector of integers constituting the factor base.

  Output -> Either a vector representing the expanded prime factorization of
            the value of (z^2 mod n) or false if there is no B-smooth
            factorization with respect to the factor base."
  [z n factor-base]
  (if (prime? z)
    false)

  (let [z2 (Math/pow z 2.0)
        z2-mod-n (long (mod z2 n))]
    (loop [factorization '[]
           to-reduce z2-mod-n
           factors (vec (rseq factor-base))]

      ;; Debug output
      ;; (debug-smooth factorization to-reduce factors)
      ;; End debug output

      (cond
        (= to-reduce 1)
        (reduce-to-exponents factorization factor-base),
        (nil? (peek factors))
        false,

        :else
        (if (= (mod to-reduce (first factors)) 0)
          (recur (conj factorization (first factors))
                 (/ to-reduce (first factors))
                 factors)
          (recur factorization
                 to-reduce
                 (vec (rest factors))))))))

(defn optimal-b-value
  "Given an integer, n, this function will employ the relation exp(sqrt(log n log log n))
  to come up with an ideal, optimized B-value to define the factor base to be used in
  determining the B-smoothness of a candidate z value.

  Input <- n, an integer, or long, potentially very large.

  Output -> An integer, ceil'd from the floating point result of the calculation"
  [n]
  (Math/ceil (Math/exp (Math/sqrt (* (Math/log10 n)
                                     (Math/log10 (Math/log10 n)))))))

(defn generate-b-smooths
  "Given a value of n, will compute an optimal B-value, the associated
  factor base for that B-value and n, followed by the number of B-smooth
  numbers with respect to n that the algorithm needs to compute in order
  to factorize n. The function will then generate enough of these b-smooth
  relations and then return a map in which the keys are the z values used
  to generate the small prime factorizations, mapped to those factorizations.

  Input <- n, the integer to be factorized.

  Ouput -> smooth-factorizations, a map where the keys are the z values and
           the vals are exponent vectors that satisfy the relation z^2 mod n
           with respect to the factor base generated by the function. This may
           return a map smaller than that of desired-smooth-numbers if we stumble
           upon a relation whose b-smooth factorization consists only of even
           exponents."
  [n]
  (let [factor-base (nt/generate-primes-to (optimal-b-value n)),
        desired-smooth-numbers (inc (count factor-base)),
        constant-map {:constants {:factorbase factor-base,
                                  :n n}}]
    (loop [smooth-factorizations {},
           smooth-counter 0,
           z (nt/random-in-range (Math/ceil (Math/sqrt n)) n)]
      (let [maybe-smooth (b-smooth? z n factor-base)]
        (cond
          (and maybe-smooth (even-exponents? (reduce-to-binary-exponents maybe-smooth)))
          (assoc constant-map
                 :even (hash-map z maybe-smooth)
                 :odd smooth-factorizations)

          (= smooth-counter desired-smooth-numbers)
          (merge constant-map {:odd smooth-factorizations})

          maybe-smooth
          (recur (assoc smooth-factorizations z maybe-smooth)
                 (inc smooth-counter)
                 (nt/random-in-range (Math/ceil (Math/sqrt n)) n))

          :else
          (recur smooth-factorizations
                 smooth-counter
                 (nt/random-in-range (Math/ceil (Math/sqrt n)) n)))))))

(defn test-for-divisor
  "Given a relation of two squares in the form of a random z value and a
  1-d matrix, the first element of which being the exponents, the second
  being the factor base, will check for the existence of a non-trivial
  divisor for n i.e. not 1.

  Input -> m, a map, comprised of a nested map of :constants consisting of them
          :factorbase and :n value, with the second key/value pair being a vector
          of the z value at index 0 and the vector of exponents at index 1.

  Output -> A vector consisting of one or two non-trivial divisors of n,
            or the vector [1 1]"
  [m]
  (let [factor-base (:factorbase (:constants m))
        n (:n (:constants m))
        exponents (-> m
                      :even
                      vals
                      first)
        a (-> m
              :even
              keys
              first)
        b (Math/sqrt (reduce *
                             (map (fn [x y]
                                    (Math/pow x y))
                                  factor-base
                                  exponents)))]

    ;; (println "Congruence of squares between: " a "and" b)

    [(int (nt/gcd (+ a b) n))
     (int (nt/gcd (- a b) n))]))

;; Woe is me, I am incomplete. I am undone
(defn matrix-computation
  "Given a matrix consisting of the constants factor-base and n, will attempt to
  find a linear dependence between the rows of exponents and, from there,
  locating a congruence of squares from which we can derive, hopefully, a non
  trivial pair of factors.

  Input <- m, a map, comprised of a nested map of :constants consisting of them
          :factorbase and :n value, the remaining elements of the map should be
          pairs of a z value and its exponent vector.

  Output -> A vector consisting of one or two non-trivial divisors of n,
            otherwise containing n and the trivial case (1)."
  [m]
  (let [factor-base (:factorbase (:constants m))
        n (:n (:constants m))
        z-values (into [] (-> m
                              :odd
                              keys))
        exponents (into [] (map reduce-to-binary-exponents
                                   (-> m
                                       :odd
                                       vals)))]
    (println "Z-values: " z-values ", Exponents: " exponents)))

(defn test-primality
  "Provided a map produced by generate-b-smooths, will apply test-for-divisor
  and test the returned elements for primality.

  Input <- A map produced by generate-b-smooths which will contain the n value
           we are attempting to factor, the factor base for the factorization,
           as well as either 1 key/value pair of z and an exponent vector or
           (inc (count factor-base)) pairs of the same.

  Output -> A set of prime factors of n. These may be trivial."
  [m]
  (into #{} (filter integer?
                    (map prime? (test-for-divisor m)))))

;; Depreciated, may not be needed.
;; (defn collect-odds
;;   "A simple map function that will collect b-smooth factorizations with odd
;;   exponents and conj them to a list of vectors.

;;   Input <- storage, a list that contains the vectors of z and the associated exponents.
;;            source, a map that contains a submap of key/value pairs corresponding to z
;;            values with odd exponent vectors.

;;   Output -> a new list, containing the elements of storage, as well as those that were
;;             found in source."
;;   [storage source]
;;   (map (fn [v]
;;          (s/union storage v))
;;        source))

(defn accumulate-primes
  "Accumulate primes, when given an integer n, will attempt to find congruences
  of squares by means of the function generate-b-smooths. If it finds a congruence
  with an even exponent vector, it will terminate its search for more b-smooth
  relations and test to see if that value provides a prime factor of n. It will
  also store the b-smooth values generated up until that point should they be needed
  for a matrix calculation.

  If generate-b-smooths returns a map with greater than 1 z-value/exponent
  vector pairs, the map will be passed to matrix-computation to search for
  prime factors.E matrix-computation will return a set containing one or two
  prime factors, should they be found. If so, they are added to the set of
  prime factors, prime-factorization, which is the function's output.

  Input <- n, an integer.

  Output -> a set of all prime numbers that compose the given integer n."
  [n]
  (loop [to-reduce (long n),
         prime-factorization #{}
         odd-exponents #{}]

    (if (<= to-reduce 1)
      prime-factorization

      (let [smooth-factorization (generate-b-smooths to-reduce)
            constants (:constants smooth-factorization)]
        (cond
          (>= (count odd-exponents) (inc (count (nt/generate-primes-to (optimal-b-value n)))))
          (let [oddmap (hash-map :constants constants,
                                 :odd odd-exponents)
                potential-primes (test-primality (matrix-computation oddmap))]
            (println oddmap))

          (nil? (:even smooth-factorization))
          (recur to-reduce
                 prime-factorization
                 (s/union odd-exponents (into #{} (:odd smooth-factorization)))),

          (:even smooth-factorization)
          (let [potential-primes (test-primality smooth-factorization)]
            (println odd-exponents)
            (if potential-primes
              (recur (reduce / to-reduce potential-primes)
                     (s/union prime-factorization potential-primes)
                     (s/union odd-exponents (:odd smooth-factorization)))
              (recur to-reduce
                     prime-factorization
                     (s/union odd-exponents (:odd smooth-factorization))))),
          :else
          nil
          )))))


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
  (loop [b-smooths (generate-b-smooths n)]
    (if (= 1 (count (rest b-smooths)))
      (test-for-divisor b-smooths)
      (println "ehhhhhh"))))
