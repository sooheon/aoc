(ns aoc.2019.day07
  (:require [aoc.utils :as u]
            [clojure.string :as str]
            [clojure.math.combinatorics :as combo]
            [clojure.core.async :as a
             :refer [>! <! >!! <!! go chan buffer close! alts!]]))

(defn parse-opcode [n]
  {:param-modes (reverse (format "%03d" (quot n 100)))
   :opcode (rem n 100)})

(defn init
  "Initialize state from string input. Can queue up inputs to program as 2nd
   argument."
  ([s] (init s []))
  ([s inputs]
   (let [c {:memory (->> (str/split s #",")
                         (map str/trim)
                         (map u/->int)
                         vec)
            :pointer 0
            :halted? false
            :out (chan 1024)
            :in (chan 1024)}]
     (when (not-empty inputs)
       (doseq [i inputs]
         (>!! (:in c) i)))
     c)))

(defn step
  [{:keys [memory pointer in out] :as st}]
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
      3 (-> (assoc-in st [:memory ptr1] (<!! in))
            (update :pointer + 2))
      4 (do
          (>!! out (memory ptr1))
          (update st :pointer + 2))
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
  (a/go-loop [c state]
    (if (:halted? c)
      c
      (recur (step c)))))

(defn amplify-with-phases [prog phases]
  (let [amps (map #(init prog [%]) phases)
        [a b c d e] amps
        [A & _] (map compute amps)]
    (>!! (:in a) 0)
    (a/pipe (:out a) (:in b))
    (a/pipe (:out b) (:in c))
    (a/pipe (:out c) (:in d))
    (a/pipe (:out d) (:in e))
    (a/pipe (:out e) (:in a))
    ;; the final output of E goes to input of A
    (<!! (:in (<!! A)))))

(comment
 (let [prog (u/input 2019 7)]
   (->> (combo/permutations (range 5))
        (map #(amplify-with-phases prog %))
        (apply max)))
 ;; ans: 79723
 (let [prog (u/input 2019 7)]
   (->> (combo/permutations (map #(+ 5 %) (range 5)))
        (map #(amplify-with-phases prog %))
        (apply max))))
;; ans: 70602018
