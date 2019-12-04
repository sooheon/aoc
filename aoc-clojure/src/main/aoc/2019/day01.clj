(ns aoc.2019.day01
  (:require
   [aoc.utils :as u]
   [clojure.string :as str]))

(def i (->> (u/input 2019 1)
            str/split-lines
            (map #(Integer/parseInt %))))

(defn fuel [mass]
  (- (Math/floor (/ mass 3)) 2))

;; ans
(reduce + (map fuel i))

(defn all-fuel [mass]
  (->> (iterate fuel mass)
       rest
       (take-while pos?)
       (reduce +)))

;; ans2
(reduce + (map all-fuel i))
