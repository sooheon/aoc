(ns aoc.2019.day06-test
  (:require [clojure.test :refer :all]
            [aoc.2019.day06 :refer :all]
            [clojure.string :as str]))

(deftest edge-map-test
  (is (= '{"COM" ("B"),
           "B" ("G" "C"),
           "C" ("D"),
           "D" ("I" "E"),
           "E" ("J" "F"),
           "G" ("H"),
           "J" ("K"),
           "K" ("L")}
         (edge-map example))))

(deftest orbit-map-test
  (is (= (orbit-map example)
         {"COM" {"B" {"G" {"H" {}}
                      "C" {"D" {"E" {"J" {"K" {"L" {}}}
                                     "F" {}}
                                "I" {}}}}}})))


(deftest orbits-test
  (is (= 42 (count (orbits example))))
  (is (= 162816 (count (orbits i)))))
