(ns aoc.2019.intcode-computer
  (:require
   [aoc.2019.intcode-computer.ops :as ops]
   [aoc.utils :as u]
   [clojure.core.async :as a :refer [>! <! >!! <!!]]
   [clojure.string :as str]))

(def ^:dynamic *debug?* false)

(defn parse-opcode [n]
  {:opcode (rem n 100)
   :param-modes (map #(u/get-digit n %) [2 3 4])})

(defn memory-map
  "Reads program string into memory map of addr->val."
  [s]
  (->> (str/split s #",")
       (map str/trim)
       (map biginteger)
       (zipmap (range))))

(defn init
  "Initialize state from program string. :in and :out are core.async channels
   of integers. Can queue up seq of inputs as 2nd argument."
  ([s] (init s nil))
  ([s inputs]
   (let [c {:memory (memory-map s)
            :pointer 0
            :relative-base 0
            :output []
            :out (a/chan 1024)
            :in (a/chan 1024)
            :halted? false}]
     (when inputs
       (doseq [i inputs]
         (>!! (:in c) i)))
     c)))

(defn read-param [{:keys [relative-base] :as c} mode p]
  (case mode
    0 (ops/getm c p)
    1 p
    2 (+ relative-base (ops/getm c p))))

(defn current-op-and-params
  [{:keys [pointer] :as c}]
  (let [{:keys [opcode param-modes]} (parse-opcode (ops/getm c pointer))]
    (cons opcode (->> (map #(+ 1 pointer %) (range 3)) ;; next 3 addr
                      (map (partial read-param c) param-modes)))))

(defn step!
  [c]
  (let [[opcode p1 p2 p3] (current-op-and-params c)]
    (case opcode
      1 (ops/op + c p1 p2 p3)
      2 (ops/op * c p1 p2 p3)
      3 (ops/take-input! c p1)
      4 (ops/output! c p1)
      5 (ops/jump-if (complement zero?) c p1 p2)
      6 (ops/jump-if zero? c p1 p2)
      7 (ops/check-pred < c p1 p2 p3)
      8 (ops/check-pred = c p1 p2 p3)
      9 (ops/update-base c p1)
      99 (ops/halt! c))))

(defn run
  "Runs intcode computer asynchronously. Final state has key :output for any
   outputs."
  [computer]
  (a/go-loop [c computer]
    (if (:halted? c)
      c
      (recur (step! c)))))

(defn set-phases
  "Initializes prog to a list of amps with corresponding phase from phases."
  [prog phases]
  (map #(init prog [%]) phases))

(defn amplify-loop
  "After running amps A~E in a serial loop, returns the final output of E"
  [amps]
  (doseq [[from to] (partition 2 1 (concat amps (take 1 amps)))]
    (a/pipe (:out from) (:in to)))
  (>!! (:in (first amps)) 0)
  (last (:output (<!! (last (map run amps))))))
