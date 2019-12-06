(ns aoc.2019.day06
  (:require [clojure.string :as str]
            [aoc.utils :as u]
            [clojure.set :as set]))

(defn read-pairs [s]
  (->> s
       str/split-lines
       (map #(str/split % #"\)"))))

(defn edge-map
  "Mapping of parent -> children."
  [pairs]
  (reduce (fn [m [parent child]]
            (update m parent conj child))
          {}
          pairs))

(defn roots
  "Finds the root nodes for a given edge-map."
  [p->c]
  (let [children (set (mapcat val p->c))]
    (set (keys (remove (comp children key) p->c)))))

(defn subtree
  "Recursively constructs a tree of nested maps from parent->child starting at
   the given node."
  [node edge-map]
  (->> (for [child (edge-map node)]
         [child (subtree child edge-map)])
       (into {})))

(defn draw-map [roots edges]
  (into {} (apply concat (for [root roots]
                           {root (subtree root edges)}))))

(defn orbit-map
  [orbit-pairs]
  (let [edges (edge-map orbit-pairs)
        roots (roots edges)]
    (draw-map roots edges)))

(defn reverse-orbit-map
  [orbit-pairs]
  (let [reverse-edges (edge-map (map reverse orbit-pairs))
        nodes (set (flatten orbit-pairs))
        non-roots (set/difference nodes (roots (edge-map orbit-pairs)))]
    (draw-map non-roots reverse-edges)))

(defn orbits
  "Returns a list of all direct and indirect orbits represented as nested map
   of child -> parent."
  [orbit-pairs]
  (->> (reverse-orbit-map orbit-pairs)
       vals
       (mapcat #(tree-seq map? vals %))
       (remove empty?)))

;;ans
(def example (read-pairs "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L"))
(def i (read-pairs (u/input 2019 6)))

;; part 2
(defn key-seq
  "Returns a sequence of keys in map."
  [m]
  (flatten (for [k (keys m)] [k (key-seq (m k))])))

(defn shortest-path [reverse-orbit-map a b]
  (let [ancestors (->> [a b]
                       (map #(get reverse-orbit-map %))
                       (map key-seq))
        common (apply set/intersection (map set ancestors))
        cts (map #(take-while (complement common) %)
                  ancestors)]
    (map count cts)))

(shortest-path (reverse-orbit-map example) "K" "I")
(shortest-path (reverse-orbit-map i) "YOU" "SAN")
