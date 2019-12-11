(ns aoc.2019.day11
  (:require [aoc.utils :as u]
            [aoc.2019.intcode-computer :as ic]
            [clojure.core.async :as a :refer [>!! <!! >! <!]]
            [clojure.string :as str]))

(def movement [[0 -1] [1 0] [0 1] [-1 0]])        ;; n, e, s, w

(defn new-heading [heading command]
  (let [new-h (-> (case command
                    0 (dec heading)
                    1 (inc heading))
                  (mod 4))]
    new-h))

(defn move [position heading]
  (mapv + position (movement heading)))

(defn color [{:keys [position board]}]
  (get-in board (reverse position) 0))

(defn paint-and-move
  [{:keys [position heading] :as hull} new-color dir-command]
  (let [new-h (new-heading heading dir-command)]
    (-> hull
        (assoc-in (cons :board (reverse position)) new-color)
        (update :position move new-h)
        (assoc :heading new-h)
        (update :visited conj position))))

(defn draw [{:keys [board] :as hull}]
  (let [ys (keys board)
        xs (mapcat keys (vals board))]
    (for [row (range (apply min ys) (inc (apply max ys)))]
      (println
       (str/join
        (map #(case (get-in board [row %] 0)
                0 \.
                1 \#)
             (range (apply min xs) (inc (apply max xs)))))))))

(defn run-painter
  ([program] (run-painter program {}))
  ([program hull]
   (let [painter (ic/init program)
         result (ic/run painter)
         io (a/go-loop [hull (merge {:position [0 0]
                                     :heading 0
                                     :visited #{}
                                     :board {}} hull)]
              (>! (:in painter) (color hull))
              (let [new-color (<! (:out painter))
                    dir-command (<! (:out painter))]
                (if (and new-color dir-command)
                  (recur (paint-and-move hull new-color dir-command))
                  hull)))]
     [result io])))

(comment
 ;; part 1
 (let [[res io] (run-painter (u/input 2019 11))]
   (count (:visited (<!! io))))

 ;; part 2
 (let [[res io] (run-painter (u/input 2019 11)
                             {:board {0 {0 1}}})]
   (draw (<!! io))))
