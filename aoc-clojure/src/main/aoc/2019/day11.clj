(ns aoc.2019.day11
  (:require [aoc.utils :as u]
            [aoc.2019.intcode-computer :as ic]
            [clojure.core.async :as a :refer [>!! <!! >! <!]]
            [clojure.string :as str]))

(def movement [[0 -1] [1 0] [0 1] [-1 0]])        ;; n, e, s, w

(defn new-heading
  "NESW -> 0123"
  [heading command]
  (-> (case command
        0 (dec heading)
        1 (inc heading))
      (mod 4)))

(defn move [position heading]
  (mapv + position (movement heading)))

(defn paint-and-move
  [{:keys [position heading] :as hull} new-color dir-command]
  (let [new-h (new-heading heading dir-command)]
    (-> hull
        (assoc-in (cons :board position) new-color)
        (update :visited conj position)
        (update :position move new-h)
        (assoc :heading new-h))))

(defn color [{:keys [position board]}]
  (get-in board position 0))

(defn run-painter
  ([prog-str] (run-painter prog-str {}))
  ([prog-str hull]
   (let [painter (ic/init prog-str)]
     (ic/run painter)
     (a/go-loop [hull (merge {:position [0 0]
                              :heading 0
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
 (count (:visited (<!! (run-painter (u/input 2019 11)))))

 ;; part 2
 (draw (<!! (run-painter (u/input 2019 11) {:board {0 {0 1}}}))))
