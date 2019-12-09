(ns aoc.2019.intcode-computer.ops
  (:require [clojure.core.async :as a :refer [<!! >!!]]))

(defn getm
  "Gets value at pointer in memory, default 0"
  [c p]
  (get-in c [:memory p] 0))

(defn halt! [{:keys [in out] :as computer}]
  (a/close! in)
  (a/close! out)
  (assoc computer :halted? true))

(defn op [f c p1 p2 p3]
  (-> (assoc-in c [:memory p3] (f (getm c p1) (getm c p2)))
      (update :pointer + 4)))

(defn take-input! [c p1]
  (-> (assoc-in c [:memory p1] (<!! (:in c)))
      (update :pointer + 2)))

(defn output! [c p1]
  (>!! (:out c) (getm c p1))
  (-> (update c :output conj (getm c p1))
      (update :pointer + 2)))

(defn jump-if [pred c p1 p2]
  (if (pred (getm c p1))
    (assoc c :pointer (getm c p2))
    (update c :pointer + 3)))

(defn check-pred [pred c p1 p2 p3]
  (-> (assoc-in c [:memory p3] (if (pred (getm c p1) (getm c p2)) 1 0))
      (update :pointer + 4)))

(defn update-base [c p1]
  (-> (update c :relative-base + (getm c p1))
      (update :pointer + 2)))


