(ns aoc.2019.day10-test
  (:require [clojure.test :refer :all]
            [aoc.2019.day10 :refer :all]))

(def x (parse ".#..#\n.....\n#####\n....#\n...##"))
(def x1 (parse "......#.#.\n#..#.#....\n..#######.\n.#.#.###..\n.#..#.....\n..#....#.#\n#..#....#.\n.##.#..###\n##...#..#.\n.#....####"))
(def x2 (parse "#.#...#.#.\n.###....#.\n.#....#...\n##.#.#.#.#\n....#.#.#.\n.##..###.#\n..#...##..\n..##....##\n......#...\n.####.###."))
(def x3 (parse ".#..#..###\n####.###.#\n....###.#.\n..###.##.#\n##.##.#.#.\n....###..#\n..#.#..#.#\n#..#.#.###\n.##...##.#\n.....#.#.."))

(deftest find-max-test
  (is (= [[3 4] 8] (max-visibility x)))
  (is (= [[5 8] 33] (max-visibility x1)))
  (is (= [[1 2] 35] (max-visibility x2)))
  (is (= [[6 3] 41] (max-visibility x3))))


