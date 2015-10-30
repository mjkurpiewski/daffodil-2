(ns projecteuler.problem4
  (require [clojure.string :as s]))

;; A palindromic number reads the same both ways. The largest palindrome made from
;; the product of two 2-digit numbers is 9009 = 91 × 99.
;; Find the largest palindrome made from the product of two 3-digit numbers.

;; For posterity, I elected to save the different iterations that my predicate
;; for ascertaining whether a number was palindromic, mainly because I think it's
;; kind of interesting how one approaches the idea of what a palindrome is, either
;; something that reads the same front-to-back as back-to-front, (which would imply
;; the vastly more simple string reversal solution) or whether or not that the two
;; halves are mirror images of each other (kind of represented by my first function,
;; on lines 17-37.) thanks to alligator and gizmo for their thoughtful insights as
;; I wrote this solution.

;; (defn is-palindromic?
;;   "This function takes an integer of indeterminant size and will return true if
;;   the number is a palindrome, false otherwise. The function operates by converting
;;   our target integer to a string, splitting the string in half, if it is even.
;;   Otherwise, we will select the center element and discard it in the odd case,
;;   splitting it in to (1 .. (p - 1)) and ((p + 1) .. n), where p is the pivot
;;   or center element.

;;   In either case, armed with our two substrings of equal length, we can reverse
;;   either of the two substrings and then perform a test for equality. If both substrings
;;   are equal after the reversal of one, our number is palindromic and our work is done.

;;   Input <- n, an integer
;;   Output -> boolean"
;;   [n]
;;   (let [string-representation (str n)
;;         string-length (count string-representation)
;;         string-split (Math/floor (/ string-length 2))
;;         first-half (take string-split string-representation)
;;         second-half (take-last string-split string-representation)]
;;     (println "First half: " first-half " Second half: " second-half)))

;; (defn is-palindromic?
;;   "Given an integer, create a string, creates a sequence representing the string,
;;   reverse that sequence, check for equality.

;;   Input <- n, an integer
;;   Output -> boolean"
;;   [n]
;;   (let [sequence-form (seq (str n))]
;;     (if (= sequence-form (reverse sequence-form))
;;       true
;;       false)))

(defn is-palindromic?
  "Given an integer, create a string from it, and compare the equality of the string
  and the result of clojure.string/reverse on said string.

  Input <- n, an integer
  Output -> boolean"
  [n]
  (let [string-form (str n)]
    (if (= string-form (s/reverse string-form))
      true
      false)))

(defn create-lower-bound
  "Given an integer n, representing the number of digits of the output, returns the
  smallest integer value of digit length n.

  Input <- n, an integer representing the number of digits in the lower boundary
  Output -> the lower bound of length n"
  [n]
  (read-string (s/replace-first (-> (repeat n 0)
                                    s/join)
                                #"0"
                                "1")))

(defn create-upper-bound
  "Given an integer n, representing the number of digits of the output, returns the
  largest integer value of digit length n.

  Input <- n, an integer representing the number of digits in the upper boundary
  Output -> the upper bound of length n"
  [n]
  (-> (repeat n 9)
      s/join
      read-string))

(defn generate-palindrome-set
  "Given an n value, which represents the largest factors, in digit length, that can
  compose a palindrome of interest. We generate a range from twice the lower and
  upper bounds, representing the minimal and maximal sizes that the products can be.
  A lazy sequence filtered by a predicate that tests for whether a number is a palindrome
  is returned as the set of palindromes in range.

  Input <- n, the number of digits that comprise all possible factors of our
           desired range
  Output -> A lazy sequence of palindromes in decreasing order"
  [n]
  (filter is-palindromic?
          (range (create-upper-bound (* 2 n))
                 (create-lower-bound (* 2 n))
                 -1)))

(defn find-largest-palindromic-product
  "Given an n value, representing the maximum length of the two factors that will
  potentially yield a palindromic product, locate the largest product of two n length
  factors that is a palindrome.

  Input <- n, an integer representing the length of the factors
  Output -> an integer that is the largest palindromic product of two factors
            defined by the input n."
  [n]
  (let [init-lower-bound (create-lower-bound n),
        init-upper-bound (create-upper-bound n)]
    (loop [trial-divisor init-upper-bound,
           palindrome-set (generate-palindrome-set n)]
      (cond
        (= (count (str (/ (first palindrome-set) trial-divisor))) n)
        (first palindrome-set),

        (= init-lower-bound trial-divisor)
        (recur init-upper-bound
               (next palindrome-set)),

        (nil? palindrome-set)
        (recur init-upper-bound
               (generate-palindrome-set (- n 1))),

        (rational? (/ (first palindrome-set) trial-divisor))
        (recur (dec trial-divisor)
               palindrome-set),

        :else
        (recur (dec trial-divisor)
               (next palindrome-set))))))
