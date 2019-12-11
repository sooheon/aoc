(ns aoc.2019.day11-test
  (:require [clojure.test :refer :all]
            [aoc.2019.day11 :refer :all]))

(deftest run-painter-test
  (is (= 6 (-> {:position [0 0]
                :heading 0
                :visited #{}
                :board {}}
               (paint-and-move 1 0)
               (paint-and-move 0 0)
               (paint-and-move 1 0)
               (paint-and-move 1 0)
               (paint-and-move 0 1)
               (paint-and-move 1 0)
               (paint-and-move 1 0)
               :visited
               count))))

(let [heading (rand-int 4)
      direction-command (rand-int 2)]
  (move [0 0] (new-heading heading direction-command)))
