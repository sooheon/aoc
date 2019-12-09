(ns aoc.utils-test
  (:require [clojure.test :refer :all]
            [aoc.utils :refer :all]))

(deftest manhattan-test
  (are [a b] (= a b)
       (manhattan [0 0]) 0
       (manhattan [1 1]) 2
       (manhattan [1 1 1]) 3
       (manhattan [0 0] [100 100]) 200))


(deftest digit-test
  (is (= 4 (get-digit 1234.567 0)))
  (is (= 3 (get-digit 1234.567 1)))
  (is (= 2 (get-digit 1234.567 2)))
  (is (= 1 (get-digit 1234.567 3)))
  (is (= 5 (get-digit 1234.567 -1)))
  (is (= 6 (get-digit 1234.567 -2)))
  (is (= 7 (get-digit 1234.567 -3))))
