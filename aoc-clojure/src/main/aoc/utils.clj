(ns aoc.utils
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.data.priority-map :refer [priority-map]]))

(defn input
  [season day]
  (-> (format "%s/day%02d" season day)
      io/resource
      slurp
      str/trim))

(defn manhattan
  "Calculates the manhattan distance between points, or from the origin. Works
   for points of any dimensionality."
  ([pt] (manhattan pt (repeat (count pt) 0)))
  ([pta ptb]
   (assert (= (count pta) (count ptb))
           "Points must have same dimensions to calculate manhattan distance.")
   (reduce + (map #(Math/abs ^Integer %) (map - pta ptb)))))

(defn ->int [s]
  (when s (Integer/parseInt s)))

(defn get-digit
  "Gets the ith digit of n. Works for decimal digits as well.

  (digit 1234 0) => 4
  (digit 1234.5 -1) => 5"
  [n i]
  (int (rem (/ (bigdec n)
               (bigdec (Math/pow 10 i)))
            10)))

(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn lcm [nums]
  (letfn [(increment-least-by [a b]
            (let [idx (.indexOf a (apply min a))]
              (update a idx + (get b idx))))]
    (loop [n nums]
      (if (apply = n)
        (first n)
        (recur (increment-least-by n nums))))))

(defn get-some
  "Returns first elem in coll for which (pred elem) returns true."
  [pred coll]
  (some #(if (pred %)
           %)
        coll))

(defn map-vals
  [m f]
  (reduce-kv
   (fn [m k v]
     (assoc m k (f v)))
   {} m))

(defn remove-keys [m pred]
  (select-keys m (remove pred (keys m))))

(defn djikstra
  "Computes single-source shortest path distances in a directed graph.

  Given a node n, (f n) should return a map with the successors of n as keys
  and their (non-negative) distances from n as vals.

  Returns a map from nodes to their distance from start."
  [start f]
  (loop [queue (priority-map start 0)
         result {}]
    (if-let [[v d] (peek queue)]
      (let [dist (-> (f v)
                     (remove-keys result)
                     (map-vals (partial + d)))]
        (recur (merge-with min (pop queue) dist)
               (assoc result v d)))
      result)))
