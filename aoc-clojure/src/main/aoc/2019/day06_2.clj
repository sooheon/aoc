(ns aoc.2019.day06-2
  (:require [aoc.2019.day06 :as day06]
            [aoc.utils :as u]))

(def i (day06/read-pairs (u/input 2019 6)))
(def ->parent (into {} (map (comp vec reverse) i)))
(def nodes (set (flatten i)))

(defn ancestors [->parent node]
  (loop [n node anc []]
    (if-let [p (->parent n)]
      (recur p (conj anc p))
      anc)))

;; pt1
(->> (for [n nodes]
       (count (ancestors ->parent n)))
     (apply +))

;; pt2
(let [a (ancestors ->parent "YOU")
      b (ancestors ->parent "SAN")]
  (+ (count (take-while (complement (set b)) a))
     (count (take-while (complement (set a)) b))))
