(ns aoc.2019.day05
  (:require [aoc.utils :as u]
            [clojure.string :as str]))

(defn init-state
  "Initialize state from string input. Can give user input to program as 2nd
   argument."
  ([s] (init-state s nil))
  ([s input]
   {:memory (->> (str/split s #",")
                 (map str/trim)
                 (map u/->int)
                 vec)
    :pointer 0
    :halted? false
    :in input}))

(defn parse-opcode [n]
  {:opcode (rem n 100)
   :param-modes (reverse (format "%03d" (quot n 100)))})

(defn step
  [{:keys [memory pointer in] :as st}]
  (let [{:keys [opcode param-modes]} (parse-opcode (memory pointer))
        [ptr1 ptr2 ptr3] (map (fn [param-mode ptr]
                                (case param-mode
                                  \0 (get memory ptr)
                                  \1 ptr))
                              param-modes
                              (map #(+ pointer %) (range 1 4)))]
    (case opcode
      99 (assoc st :halted? true)
      1 (-> (assoc-in st [:memory ptr3] (+ (memory ptr1) (memory ptr2)))
            (update :pointer + 4))
      2 (-> (assoc-in st [:memory ptr3] (* (memory ptr1) (memory ptr2)))
            (update :pointer + 4))
      3 (-> (assoc-in st [:memory ptr1] (or in (u/->int (read-line))))
            (update :pointer + 2))
      4 (-> (assoc st :out (memory ptr1))
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
 (:out (compute (init-state i 1)))
 ;; ans 2
 (:out (compute (init-state i 5))))
