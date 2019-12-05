(ns aoc.2019.day05
  (:require [aoc.utils :as u]
            [clojure.string :as str]))

(defn init-state
  "Initialize state from string input. Can give user input to program as 2nd
   argument."
  ([s] (init-state s nil))
  ([s input]
   (let [memory (->> (str/split s #",")
                     (map str/trim)
                     (map u/->int))]
     {:memory (vec memory) :pointer 0 :halted? false :input input})))

(defn parse-opcode [n]
  {:opcode (rem n 100)
   :param-modes (->> (quot n 100)
                     (format "%03d")
                     reverse)})

(defn step
  [{:keys [memory pointer input] :as st}]
  (let [{:keys [opcode param-modes]} (parse-opcode (memory pointer))
        [ptr1 ptr2 ptr3] (->> (range 1 4)
                              (map #(+ pointer %))
                              (map (fn [param-mode param]
                                     (case param-mode
                                       \0 (get memory param)
                                       \1 param))
                                   param-modes))]
    (case opcode
      99 (assoc st :halted? true)
      1 (-> (assoc-in st [:memory ptr3] (+ (memory ptr1) (memory ptr2)))
            (update :pointer + 4))
      2 (-> (assoc-in st [:memory ptr3] (* (memory ptr1) (memory ptr2)))
            (update :pointer + 4))
      3 (-> (assoc-in st [:memory ptr1] (or input (u/->int (read-line))))
            (update :pointer + 2))
      4 (-> (assoc st :output (memory ptr1))
            (update :pointer + 2))
      5 (if-not (zero? (memory ptr1))
          (assoc st :pointer (memory ptr2))
          (update st :pointer + 3))
      6 (if (zero? (memory ptr1))
          (assoc st :pointer (memory ptr2))
          (update st :pointer + 3))
      7 (-> (assoc-in st [:memory ptr3] (if (< (memory ptr1) (memory ptr2)) 1 0))
            (update :pointer + 4))
      8 (-> (assoc-in st [:memory ptr3] (if (= (memory ptr1) (memory ptr2)) 1 0))
            (update :pointer + 4)))))

(defn compute [state]
  (if (:halted? state)
    state
    (recur (step state))))

(comment
 (def i (u/input 2019 5))
 ;; ans 1
 (:output (compute (init-state i 1)))
 ;; ans 2
 (:output (compute (init-state i 5))))
