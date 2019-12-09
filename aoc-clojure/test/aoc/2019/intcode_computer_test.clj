(ns aoc.2019.intcode-computer-test
  (:require [clojure.test :refer :all]
            [aoc.2019.intcode-computer :refer :all]
            [clojure.core.async :refer [<!!]]
            [clojure.string :as str]
            [aoc.utils :as u]
            [clojure.math.combinatorics :as combo]))

(deftest compute-test
  (are
    [a b] (= a b)
    "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"
    (str/join "," (:output (<!! (run (init "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99")))))

    (-> (<!! (run (init "1102,34915192,34915192,7,4,7,99,0")))
        :output
        first
        str
        count)
    16

    (last (:output (<!! (run (init (u/input 2019 5) [1])))))
    13818007

    (last (:output (<!! (run (init (u/input 2019 5) [5])))))
    3176266

    (-> (<!! (run (init "104,1125899906842624,99")))
        :output
        first)
    1125899906842624

    (let [c (init (u/input 2019 9) [2])]
      (->> (run c)
           <!! :output first))
    78869))

(deftest amplify-loop-test
  (is (= (amplify-loop (set-phases "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0" [4 3 2 1 0]))
         43210))
  (is (= (amplify-loop (set-phases "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,\n27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"
                                   [9 8 7 6 5]))
         139629729))
  (is (= (amplify-loop (set-phases "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,\n-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,\n53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10"
                                   [9 7 8 5 6]))
         18216))

  (is (= (let [prog (u/input 2019 7)]
           (->> (combo/permutations [0 1 2 3 4])
                (map #(set-phases prog %))
                (map amplify-loop)
                (apply max)))
         79723))

  (is (= (let [prog (u/input 2019 7)]
           (->> (combo/permutations [5 6 7 8 9])
                (map #(set-phases prog %))
                (map amplify-loop)
                (apply max)))
         70602018)))
