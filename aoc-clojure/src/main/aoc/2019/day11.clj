(ns aoc.2019.day11
  (:require [aoc.utils :as u]
            [aoc.2019.intcode-computer :as ic]
            [clojure.core.async :as a :refer [<!! >! <!]]
            [clojure.string :as str]))


(defn turn [[x y] command]
  (case command
    0 [y (* -1 x)]
    1 [(* -1 y) x]))

(defn paint-and-move
  [{:keys [position heading] :as hull} color turn-cmd]
  (let [new-h (turn heading turn-cmd)]
    (-> hull
        (assoc-in (cons :board position) color)
        (update :visited conj position)
        (update :position (partial map +) new-h)
        (assoc :heading new-h))))

(defn color [{:keys [position board]}]
  (get-in board position 0))

(defn run-painter
  ([prog-str] (run-painter prog-str {}))
  ([prog-str hull]
   (let [painter (ic/init prog-str)]
     (ic/run painter)
     (a/go-loop [hull (merge {:position [0 0]
                              :heading [0 -1]
                              :visited #{}
                              :board {}} hull)]
       (>! (:in painter) (color hull))
       (let [new-color (<! (:out painter))
             dir-command (<! (:out painter))]
         (if (and new-color dir-command)
           (recur (paint-and-move hull new-color dir-command))
           hull))))))

(defn draw [{:keys [board]}]
  (let [xs (keys board)
        ys (mapcat keys (vals board))
        [x-range y-range] (map #(range (apply min %)
                                       (inc (apply max %)))
                               [xs ys])]
    (for [row y-range]
      (println
       (str/join (map #(case (get-in board [% row] 0)
                         0 \space
                         1 \#)
                      x-range))))))

(comment
 ;; part 1
 (time (count (:visited (<!! (run-painter (u/input 2019 11))))))
 (draw (<!! (run-painter (u/input 2019 11))))
 ;; part 2
 (draw (<!! (run-painter (u/input 2019 11) {:board {0 {0 1}}}))))
