(ns aoc.2019.day02
  (:require [aoc.utils :as u]
            [clojure.string :as str]))

(defn parse-input [s]
  (->> (str/split (str/trim s) #",")
       (mapv #(Integer/parseInt %))))

(def i (parse-input (u/input 2019 2)))

(defn run-instruction
  [memory pointer]
  (let [[instr p1 p2 out] (->> [0 1 2 3]
                               (map #(+ pointer %))
                               (map #(get memory %)))]
    (assoc memory out (({1 + 2 *} instr)
                       (get memory p1)
                       (get memory p2)))))

(time
 (for [n (range 100 (* 100 100))
       i (range n)]
   (get n i)))

(defn step
  [{:keys [memory pointer halted?] :as state}]
  (when-not halted?
    (case (get memory pointer)
      99 (assoc state :halted? true)
      (-> state
          (update :memory run-instruction pointer)
          (update :pointer + 4)))))

(defn final-state [state]
  (some #(when (:halted? %) (:memory %))
        (iterate step state)))

(defn run-ops
  "Pt 1"
  [s]
  (let [init {:memory (parse-input s) :pointer 0}]
    (str/join "," (final-state init))))

;; pt 2
(defn pt2 []
  (let [inputs (for [noun (range 100)
                     verb (range 100)]
                 (-> i
                     (assoc 1 noun)
                     (assoc 2 verb)))]
    (->> (shuffle inputs)
         (some #(when (= (get (final-state {:memory % :pointer 0}) 0)
                         19690720)
                  %))
         ((juxt #(get % 1) #(get % 2))))))
