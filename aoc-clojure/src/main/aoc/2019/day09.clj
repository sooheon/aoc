(ns aoc.2019.day09
  (:require [aoc.utils :as u]
            [clojure.string :as str]
            [clojure.math.combinatorics :as combo]
            [clojure.core.async :as a
             :refer [>! <! >!! <!! go chan buffer close! alts!]]))

(defn parse-opcode [n]
  {:param-modes (reverse (format "%03d" (int (quot n 100))))
   :opcode (rem n 100)})

(defn init
  "Initialize state from string input. Can queue up inputs to program as 2nd
   argument."
  ([s] (init s []))
  ([s inputs]
   (let [c {:memory (->> (str/split s #",")
                         (map str/trim)
                         (map biginteger)
                         (map-indexed (fn [idx v] [idx v]))
                         (into {}))
            :pointer 0
            :halted? false
            :output []
            :out (chan 1024)
            :in (chan 1024)
            :relative-base 0}]
     (when (not-empty inputs)
       (doseq [i inputs]
         (>!! (:in c) i)))
     c)))

(def ^:dynamic *debug?* false)

(defn step
  [{:keys [memory pointer in out relative-base] :as st}]
  (letfn [(getm [addr] (get memory addr 0))]
    (let [{:keys [opcode param-modes]} (parse-opcode (getm pointer))
          [ptr1 ptr2 ptr3] (->> (map #(+ pointer %) (range 1 4))
                                (map (fn [mode ptr]
                                       (case mode
                                         \0 (getm ptr)
                                         \1 ptr
                                         \2 (+ relative-base (getm ptr))))
                                     param-modes))]
      (when *debug?*
        (println
         (format "running opcode %s with args %s, modes %s"
                 opcode [ptr1 ptr2 ptr3] param-modes)))
      (case opcode
        99 (assoc st :halted? true)
        1 (-> (assoc-in st [:memory ptr3] (+ (getm ptr1) (getm ptr2)))
              (update :pointer + 4))
        2 (-> (assoc-in st [:memory ptr3] (* (getm ptr1) (getm ptr2)))
              (update :pointer + 4))
        3 (-> (assoc-in st [:memory ptr1] (<!! in))
              (update :pointer + 2))
        4 (do
            (>!! out (getm ptr1))
            (-> st
                (update :output conj (getm ptr1))
                (update :pointer + 2)))
        5 (if-not (zero? (getm ptr1))
            (assoc st :pointer (getm ptr2))
            (update st :pointer + 3))
        6 (if (zero? (getm ptr1))
            (assoc st :pointer (getm ptr2))
            (update st :pointer + 3))
        7 (-> (assoc-in st [:memory ptr3] (if (< (getm ptr1) (getm ptr2)) 1 0))
              (update :pointer + 4))
        8 (-> (assoc-in st [:memory ptr3] (if (= (getm ptr1) (getm ptr2)) 1 0))
              (update :pointer + 4))
        9 (-> (update st :relative-base + (getm ptr1))
              (update :pointer + 2))))))

(defn compute [state]
  (a/go-loop [c state]
    (if (:halted? c)
      c
      (recur (step c)))))

(def i (u/input 2019 9))

(comment
 (binding [*debug?* false]
   (let [c (init i [2])]
     (<!! (compute c)))))

