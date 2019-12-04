(ns aoc.2019.day04-draw
  (:require [aoc.2019.day04 :as day04]
            [quil.core :as q]
            [quil.middleware :as qm]))

;; cribbed from http://quil.info/sketches/show/4350261fceb6f0e6f385e23fcd9a8ca91623ffd813912ca79c47bfa51ad1a6f1

(def canvas-size
  1000
  #_(-> (count day04/input-range)
        Math/sqrt
        Math/ceil
        int
        (+ 2)))

(defn setup []
  (q/background 0)
  (q/blend-mode :add)
  {:n (dec (first day04/input-range))
   :p1? false
   :p2? false})

(defn update-state- [{:keys [n]}]
  (let [n' (inc n)]
    {:p1? (day04/password? n')
     :p2? (day04/password-2? n')
     :n n'}))

(defn update-state [state]
  (let [state' (update-state- state)]
    (if ((some-fn :p1? :p2?) state')
      state'
      (recur state'))))

(defn draw-pixel [n [r g b]]
  (let [offset-n (- n (first day04/input-range))]
    (q/stroke r g b)
    (q/fill r g b 0.5)
    (q/rect (mod offset-n canvas-size)
            (quot offset-n canvas-size)
            2 2)))

(defn draw [{:keys [p1? p2? n]}]
  (cond
    p2? (draw-pixel n [0 100 0])
    p1? (draw-pixel n [100 0 0])))

(q/defsketch passwords
  :middleware [qm/fun-mode]
  :size [canvas-size canvas-size]
  :setup setup
  :draw draw
  :update update-state)
