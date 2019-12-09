(ns aoc.2019.intcode-computer-test
  (:require [clojure.test :refer :all]
            [aoc.2019.intcode-computer :refer :all]
            [clojure.core.async :as a :refer [<!!]]
            [clojure.string :as str]
            [aoc.utils :as u]))

(def i (u/input 2019 9))

(deftest tests
  (are
    [a b] (= a b)
    "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"
    (str/join "," (:output (<!! (compute (init "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99")))))

    (-> (<!! (compute (init "1102,34915192,34915192,7,4,7,99,0")))
        :output
        first
        str
        count)
    16

    (-> (<!! (compute (init "104,1125899906842624,99")))
        :output
        first)
    1125899906842624

    (let [c (init i [2])]
      (->> (compute c)
           <!! :output first))
    78869))

