(ns aoc.2019.day04
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clj-async-profiler.core :as prof]
            [aoc.utils :as u]))

(def input-range
  (->> (str/split "245182-790572" #"-")
       (map u/->int)
       (apply range)
       (map str)))

(defn digits<= [s]
  (apply <= (map int s)))

(defn repeats [s]
  (->> (partition 2 1 s)
       (filter #(apply = %))
       (map first)
       seq))

(def password? (every-pred
                digits<=
                repeats
                #(= (count %) 6)))

(defn exact-doubles [s]
  (let [triplet-or-longer (->> (re-seq #"(\d)\1{2,}" s)
                               (map second))]
    (seq
     (set/difference
      (set (map str (repeats s)))
      (set triplet-or-longer)))))

(def password-2? (every-pred password? exact-doubles))

(comment
 (prof/serve-files 8888)
 (prof/profile
  {:return-file true}
  (dotimes [_ 10]
    ;; pt1
    (->> input-range
         (filter password?)
         count
         time)
    ;; pt2
    (->> input-range
         (filter password-2?)
         count
         time))))
