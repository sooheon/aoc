(ns aoc.2019.day09
  (:require
   [aoc.utils :as u]
   [aoc.2019.intcode-computer :as ic]
   [clojure.core.async :as a :refer [<!! >!!]]))

;; part 1
(let [c (ic/init (u/input 2019 9))]
  (>!! (:in c) 1)
  (first (:output (<!! (ic/compute c)))))

;; part 2
(let [c (ic/init (u/input 2019 9))]
  (>!! (:in c) 2)
  (first (:output (<!! (ic/compute c)))))
