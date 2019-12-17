(ns aoc.2019.day15
  (:require
   [aoc.2019.intcode-computer :as ic]
   [aoc.utils :as u]
   [clojure.core.async :as a :refer [<! >! <!! >!!]]
   [clojure.string :as str]))


(def movement {1 [0 -1]
               2 [0 1]
               3 [-1 0]
               4 [1 0]})

(defn neighbor [pos direction]
  (mapv + pos (movement direction)))

(defn find-drone [world]
  (u/get-some (comp :drone? val) world))

(defn mark-feature
  "Places tile at given direction from drone."
  [world direction tile]
  (let [[pos _] (find-drone world)]
    (-> world
        (assoc-in [(neighbor pos direction) :tile] tile))))

(defn move-drone [world direction]
  (let [[pos _] (find-drone world)
        to (neighbor pos direction)]
    (-> world
        (update-in [pos :n-traversed] (fnil inc 0))
        (assoc-in [pos :drone?] false)
        (assoc-in [to :drone?] true)
        (update-in [to :tile] #(or % \.)))))

(defn paths
  [world pos]
  (letfn [(visit [dir] [dir (get world (neighbor pos dir))])]
    (->> (map visit [1 2 3 4])
         (remove #(= \# (:tile (second %))))
         (into {}))))

(defn choose-direction
  "Chooses a random non-wall direction, preferring less explored tiles."
  [world]
  (let [[pos _] (find-drone world)
        cands (->> (paths world pos)
                   (sort-by #(get (val %) :n-traversed 0))
                   (map first))]
    (rand-nth
     (concat (repeat 4 (first cands))
             cands))))

(defn draw!
  "world maps from coordinate [x y] to a cell object with keys
  :tile, :drone, :n-traversed."
  [world]
  (let [coords (keys world)
        min-max (juxt #(apply min %) #(inc (apply max %)))
        get-range (fn [idx] (->> (map #(get % idx) coords)
                                 min-max
                                 (apply range)))
        [x-range y-range] (map get-range [0 1])]
    (print "\n\n\n\n\n")
    (->> (for [y y-range]
           (->> x-range
                (map #(let [loc (get world [% y])]
                        (cond
                          (:drone? loc) \D
                          (= [% y] [0 0]) \X
                          :else (get loc :tile \space))))
                (str/join)))
         (str/join "\n")
         println)))

(defn simulate [prog-str]
  (let [remote-con (ic/init prog-str)
        kill-ch (a/chan)]
    (ic/compute! remote-con)
    {:sim (a/go-loop [world {[0 0] {:drone? true :tile \.}}]
            (draw! world)
            (let [direction (choose-direction world)]
              (>! (:in remote-con) direction)
              (let [[val port] (a/alts! [(:out remote-con) kill-ch])]
                (if (= port kill-ch)
                  world
                  (case val
                    0 (recur (mark-feature world direction \#))
                    1 (recur (move-drone world direction))
                    2 (recur (-> world
                                 (mark-feature direction \!)
                                 (move-drone direction)))
                    world)))))
     :kill kill-ch}))


(comment
 ;;; part 1
 (def c (simulate (u/input 2019 15)))
 ;; run simulation until visually confirm map has been explored, then send kill cmd
 ;; todo: make simulation check for completeness automatically
 ;; todo: make better maze explorer than random walk
 (>!! (:kill c) true)
 (def w (<!! (:sim c)))
 (draw! w)

 ;; pt 1 answer
 (get
  (u/djikstra
   [0 0]
   (fn [node]
     (->> (paths w node)
          (map (juxt
                #(neighbor node (key %))
                (constantly 1)))
          (into {}))))
  (first (u/get-some #(= \! (:tile (val %)))
                     w)))

 ;; pt 2 answer
 (apply max (vals (u/djikstra
                   (first (u/get-some #(= \! (:tile (val %)))
                                      w))
                   (fn [node]
                     (->> (paths w node)
                          (map (juxt
                                #(neighbor node (key %))
                                (constantly 1)))
                          (into {})))))))


