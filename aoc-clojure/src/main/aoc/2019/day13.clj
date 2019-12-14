(ns aoc.2019.day13
  (:require [aoc.2019.intcode-computer :as ic]
            [aoc.utils :as u]
            [clojure.core.async :as a :refer [>!! <!! >! <!]]
            [clojure.string :as str]))

(def tile [\space \# \b \- \.])

(defn map-game [output]
  (let [instructions (partition 3 output)]
    (reduce (fn [game [x y tile-id]]
              (assoc-in game [x y] (tile tile-id)))
            {}
            instructions)))

;; ans 1
(->> (let [c (ic/init (u/input 2019 13))
           running (ic/compute! c)]
       (a/go-loop []
         (when (<! (:out c)) (recur)))
       (:output (<!! running)))
     map-game
     (mapcat (comp vals val))
     (filter #(= \b %))
     count)

(defn draw [board]
  (let [xs (keys board)
        ys (mapcat keys (vals board))]
    (when (and xs ys)
      (let [[x-range y-range] (map #(range
                                     (apply min %)
                                     (inc (apply max %)))
                                   [xs ys])]
        (->> y-range
             (map (fn [row] (str/join (map #(get-in board [% row] \space)
                                           x-range))))
             (str/join "\n"))))))

(defn coord->tile
  [game-map]
  (into {} (for [[k v] game-map
                 [K V] v]
             [[k K] V])))

(defn find-tile [coords tile]
  (->> coords
       (filter #(= tile (val %)))
       ffirst))

(defn paddle-dir [board]
  (let [coords (coord->tile board)]
    (compare (first (find-tile coords \.))
             (first (find-tile coords \-)))))

(comment
 (let [game (-> (ic/init (u/input 2019 13))
                (assoc-in [:memory 0] 2))
       pixel-chan (a/chan 32 (partition-all 3))
       board (atom {})
       score (atom nil)]
   (a/pipe (:out game) pixel-chan)
   (a/go-loop []
     (when-let [[x y t] (<! pixel-chan)]
       (if (= [x y] [-1 0])
         (reset! score t)
         (swap! board assoc-in [x y] (tile t)))
       (recur)))
   (loop [c game]
     (when-not (:halted? c)
       (println (draw @board) @score)
       (if (:pending-input? c)
         (recur (do
                  (>!! (:in c) (paddle-dir @board))
                  (ic/step! c :return-state)))
         (recur (ic/step! c :return-state)))))))

