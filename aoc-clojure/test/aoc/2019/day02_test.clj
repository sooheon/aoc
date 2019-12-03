(ns aoc.2019.day02-test
  (:require [clojure.test :refer :all]
            [aoc.2019.day02 :refer :all]))

(deftest run-ops-test
  (is (= (run-ops "1,0,0,0,99") "2,0,0,0,99"))
  (is (= (run-ops "2,3,0,3,99") "2,3,0,6,99"))
  (is (= (run-ops "2,4,4,5,99,0") "2,4,4,5,99,9801"))
  (is (= (run-ops "1,1,1,4,99,5,6,0,99") "30,1,1,4,2,5,6,0,99"))
  (is (= (run-ops "1,9,10,3,2,3,11,0,99,30,40,50") "3500,9,10,70,2,3,11,0,99,30,40,50")))
