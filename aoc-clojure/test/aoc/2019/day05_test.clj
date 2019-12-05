(ns aoc.2019.day05-test
  (:require [clojure.test :refer :all]
            [aoc.2019.day05 :refer :all]
            [aoc.utils :as u]
            [clojure.string :as str]))

(deftest day2-still-works-test
  (is (= (str/join "," (:memory (compute (init-state "1,0,0,0,99")))) "2,0,0,0,99"))
  (is (= (str/join "," (:memory (compute (init-state "2,3,0,3,99")))) "2,3,0,6,99"))
  (is (= (str/join "," (:memory (compute (init-state "2,4,4,5,99,0")))) "2,4,4,5,99,9801"))
  (is (= (str/join "," (:memory (compute (init-state "1101,100,-1,4,0")))) "1101,100,-1,4,99"))
  (is (= (let [i (init-state (u/input 2019 2))]
           (-> i
               (assoc-in [:memory 1] 12)
               (assoc-in [:memory 2] 2)
               compute
               :memory
               first))
         5534943)))

(deftest with-inputs-test
  (is (= (:output (compute (init-state "3,0,4,0,99" 77))) 77))
  (is (= (:output (compute (init-state "3,9,8,9,10,9,4,9,99,-1,8" 8))) 1))
  (is (= (:output (compute (init-state "3,9,8,9,10,9,4,9,99,-1,8" 42))) 0))
  (is (= (:output (compute (init-state "3,9,7,9,10,9,4,9,99,-1,8" 7))) 1))
  (is (= (:output (compute (init-state "3,9,7,9,10,9,4,9,99,-1,8" 8))) 0))
  (is (= (:output (compute (init-state "3,3,1108,-1,8,3,4,3,99" 8))) 1))
  (is (= (:output (compute (init-state "3,3,1107,-1,8,3,4,3,99" 7))) 1)))

(deftest long-input-test
  (let [i "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,\n1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,\n999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"]
    (is (= 999 (:output (compute (init-state i 0)))))
    (is (= 1000 (:output (compute (init-state i 8)))))
    (is (= 1001 (:output (compute (init-state i 16)))))))

(deftest parse-opcode-test
  (is (= {:param-modes [\0 \1 \0] :opcode 2}
         (parse-opcode 1002)))
  (is (= {:param-modes [\0 \0 \0] :opcode 2}
         (parse-opcode 2)))
  (is (= {:param-modes [\1 \1 \0] :opcode 1}
         (parse-opcode 1101))))

