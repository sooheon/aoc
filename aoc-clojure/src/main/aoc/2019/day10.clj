(ns aoc.2019.day10
  (:require [aoc.utils :as u]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn parse
  "Parses 2d map into set of coordinates."
  [s]
  (let [input (str/split-lines s)
        w (count (first input))
        h (count input)]
    (-> (for [x (range w)
              y (range h)
              :when (= \# (get-in input [y x]))]
          [x y])
        set)))

(defn blocked [station asteroid]
  (if (= station asteroid)
    [station]
    (let [direction (map - asteroid station)
          gcd (Math/abs ^Integer (apply u/gcd direction))
          [dx dy] (map #(/ % gcd) direction)]
      (rest (take 10 (iterate (fn [[x y]]
                                [(+ dx x) (+ dy y)])
                              asteroid))))))

(defn visible [asteroids pov]
  (set/difference
   asteroids
   (set (mapcat #(blocked pov %) asteroids))))

(defn visibility [asteroids]
  (->> asteroids
       (map (fn [x] [x (visible asteroids x)]))
       (into {})))

(defn max-visibility [asteroids]
  (->> (visibility asteroids)
       (map (juxt key (comp count val)))
       (apply max-key second)))

(defn bearing
  "North is 0, East is 90, South is 180, West is 270"
  [from to]
  (let [[dx dy] (map - to from)
        angle (- 0
                 (Math/toDegrees (Math/atan2 dx dy))
                 180)]
    (if (< angle 0)
      (+ angle 360)
      angle)))

(defn destroy-order [asteroids base]
  (loop [base base
         asteroids asteroids
         destroyed []]
    (let [targets (->> (get (visibility asteroids) base)
                       (sort-by #(bearing base %)))
          remaining (set/difference asteroids (set targets))]
      (if (empty? remaining)
        destroyed
        (recur base remaining (concat destroyed targets))))))

(comment
 (def i (u/input 2019 10))
 ;; part 1
 (def asteroids (parse i))
 (max-visibility asteroids)                       ;; [[11 11] 221]
 ;; part 2
 (def base (first (max-visibility asteroids)))

 (nth (sort-by #(bearing base %) (get (visibility asteroids) base)) 199)
 (let [a (set/difference
          asteroids
          (get (visibility asteroids)
               base))]
   (set/difference a (get (visibility a) base)))
 (destroy-order asteroids base))
;; [8 6]

