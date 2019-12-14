(ns aoc.2019.day12
  (:require [criterium.core :as crit]
            [aoc.utils :as u]
            [clojure.math.numeric-tower :as math]))

(defn init [positions]
  (map (fn [p] {:p p :v [0 0 0]}) positions))

(defn move [{:keys [p v]}]
  {:p (mapv + p v) :v v})

(defn gravitate
  ([body bodies]
   (let [dv (for [i (range (count (:p body)))]
              (let [me (get (:p body) i)
                    all (map #(get % i) (map :p bodies))
                    above (filter #(> % me) all)
                    below (filter #(< % me) all)]
                (- (count above) (count below))))]
     (update body :v (partial mapv +) dv)))
  ([bodies]
   (map #(gravitate % bodies) bodies)))

(defn step [positions]
  (->> (gravitate positions)
       (map move)))

(defn total-energy [moons]
  (letfn [(asum [xs] (reduce + (map #(Math/abs ^Integer %) xs)))]
    (reduce (fn [energy {:keys [p v]}]
              (+ energy (* (asum p) (asum v))))
            0
            moons)))

(def x1 [[-1 0 2]
         [2 -10 -7]
         [4 -8 8]
         [3 5 -1]])

(def x2 [[-8 -10 0]
         [5 5 10]
         [2 -7 3]
         [9 -8 -3]])

(def i [[3 2 -6]
        [-13 18 10]
        [-8 -1 13]
        [5 10 4]])

;; part 1
(total-energy (nth (iterate step (init i)) 1000))

;; part 2
;; find x loop
(defn axis [system i]
  (mapcat (fn [body]
            [(get-in body [:p i])
             (get-in body [:v i])])
          system))

(comment
 (let [system (init i)]
   (->> (for [ax [0 1 2]]
          (loop [sys system
                 initial (axis system ax)
                 iters 1]
            (let [next-s (step sys)]
              (if (= (axis next-s ax) initial)
                iters
                (recur next-s initial (inc iters))))))
        (reduce math/lcm))))
