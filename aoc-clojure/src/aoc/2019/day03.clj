(ns aoc.2019.day03
  (:require [aoc.utils :as u]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn read-instruction [s]
  (let [[dir & nums] s]
    [(keyword (str/lower-case dir)) (Integer/parseInt (apply str nums))]))

(defn parse [s]
  (->> s
       str/trim
       str/split-lines
       (map #(str/split % #","))
       (map #(map read-instruction %))))

(def i (parse (u/input 2019 3)))

(defn line [from [dir n]]
  (let [to (case dir
             :u (update from 1 #(+ % n))
             :d (update from 1 #(- % n))
             :l (update from 0 #(- % n))
             :r (update from 0 #(+ % n)))
        [[x X] [y Y]] (map #(sort [%1 %2]) from to)]
    (cond-> (for [xi (range x (inc X))
                  yi (range y (inc Y))]
              [xi yi])
      (#{:d :l} dir) reverse
      true rest)))

(defn wire-path [instructions]
  (-> (reduce (fn [{:keys [end] :as acc} instr]
                (let [l (line end instr)]
                  (-> acc
                      (update :path into l)
                      (assoc :end (last l)))))
              {:path [[0 0]] :end [0 0]}
              instructions)
      :path))

(defn intersection [paths]
  (->> paths
       (map set)
       (apply set/intersection)))

(defn nearest-manhattan [points]
  (->> (set/difference points #{[0 0]})
       (map u/manhattan)
       (apply min)))

(defn path-distance [paths point]
  (->> paths
       (map #(take-while (complement #{point}) %))
       (map count)
       (reduce +)))

(defn shortest-delay
  "Finds all intersections, then returns the smallest sum of path distances to
   an intersection."
  [paths]
  (->> (set/difference (intersection paths) #{[0 0]})
       (map #(path-distance paths %))
       (apply min)))

;; ans1
(-> (map wire-path i)
    intersection
    nearest-manhattan
    time)
;; "Elapsed time: 231.519547 msecs"

;; ans2
(-> (map wire-path i)
    shortest-delay
    time)
;; "Elapsed time: 722.363764 msecs"

