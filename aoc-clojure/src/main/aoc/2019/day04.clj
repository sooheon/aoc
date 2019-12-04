(ns aoc.2019.day04
  (:require [clojure.string :as str]
            [aoc.utils :as u]
            [clojure.set :as set]))

(def input-range
  (->> (str/split "245182-790572" #"-")
       (map u/->int)
       (apply range)))

(defn digits<= [n]
  (->> (map int (str n))
       (apply <=)))

(defn repeats [n]
  (->> (partition 2 1 (str n))
       (filter #(apply = %))
       (map first)
       (map str)
       seq))

(def password? (every-pred
                digits<=
                repeats
                #(= (count (str %)) 6)))

(defn exact-doubles [n]
  (let [triplet-or-longer (->> (re-seq #"(\d)\1{2,}" (str n))
                               (map second))]
    (seq
     (set/difference
      (set (repeats (str n)))
      (set triplet-or-longer)))))

;; pt1
(->> input-range
     (filter password?)
     count
     time)

;; pt2
(def password-2? (every-pred password? exact-doubles))
(->> input-range
     (filter password-2?)
     count
     time)
