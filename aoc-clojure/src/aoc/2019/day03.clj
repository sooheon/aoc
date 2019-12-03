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
  (let [[to-x to-y] (case dir
                      :u (update from 1 #(+ % n))
                      :d (update from 1 #(- % n))
                      :l (update from 0 #(- % n))
                      :r (update from 0 #(+ % n)))
        [min-x max-x] (sort [(first from) to-x])
        [min-y max-y] (sort [(second from) to-y])]
    (cond-> (for [x (range min-x (inc max-x))
                  y (range min-y (inc max-y))]
              [x y])
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

(defn intersections [wire-paths]
  (->> wire-paths
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
  "Given set of points where paths intersect, return the point where the sum
   of all paths leading to the point is smallest."
  [paths]
  (->> (set/difference (intersections paths) #{[0 0]})
       (map #(path-distance paths %))
       (apply min)))

;; ans1
(->> i
     (map wire-path)
     intersections
     nearest-manhattan
     time)
;; "Elapsed time: 271.487309 msecs"

;; ans2
(-> (map wire-path i)
    shortest-delay
    time)
;; "Elapsed time: 876.94601 msecs"

