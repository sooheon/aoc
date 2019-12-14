(ns aoc.2019.intcode-computer
  (:require
   [aoc.2019.intcode-computer.op :as op]
   [aoc.utils :as u]
   [clojure.core.async :as a :refer [>! <! >!! <!!]]
   [clojure.string :as str]))

(defn init
  "Initialize computer state from program string. :in and :out are core.async
   chans. Can queue up seq of inputs as 2nd argument."
  ([s] (init s nil))
  ([s prog-string]
   (let [c {:memory (->> (str/split s #",")
                         (map (comp biginteger str/trim))
                         (zipmap (range)))
            :pointer 0
            :relative-base 0
            :output []
            :out (a/chan 1024)
            :in (a/chan 1024)
            :halted? false}]
     (when prog-string
       (doseq [i prog-string]
         (>!! (:in c) i)))
     c)))

(defn parse-opcode [n]
  {:opcode (rem n 100)
   :param-modes (map #(u/get-digit n %) [2 3 4])})

(defn read-param [{:keys [relative-base] :as c} mode p]
  (case mode
    0 (op/getm c p)
    1 p
    2 (+ relative-base (op/getm c p))))

(defn current-op-and-params
  [{:keys [pointer] :as c}]
  (let [{:keys [opcode param-modes]} (parse-opcode (op/getm c pointer))]
    (cons opcode
          (->> (map #(+ 1 pointer %) (range 3))   ;; next 3 addr
               (map (partial read-param c) param-modes)))))

(defn step!
  ([c] (step! c :async))
  ([c mode]
   (let [[opcode p1 p2 p3] (current-op-and-params c)]
     (case opcode
       1 (op/calc + c p1 p2 p3)
       2 (op/calc * c p1 p2 p3)
       3 (do
           (case mode
             :prompt-user (do (println "give input:")
                              (>!! (:in c) (read))
                              (op/take-input! c p1))
             :return-state (if-not (:pending-input? c)
                             (assoc c :pending-input? true)
                             (op/take-input! (assoc c :pending-input? false) p1))
             :async (op/take-input! c p1)))
       4 (op/output! c p1)
       5 (op/jump-if (complement zero?) c p1 p2)
       6 (op/jump-if zero? c p1 p2)
       7 (op/compare-two < c p1 p2 p3)
       8 (op/compare-two = c p1 p2 p3)
       9 (op/move-base c p1)
       99 (op/halt! c)))))

(defn compute!
  "Runs intcode computer asynchronously. Final state has key :output to hold
   outputs, as :out channel is ephemeral."
  [computer]
  (a/go-loop [c computer]
    (if (:halted? c)
      c
      (recur (step! c)))))

(defn compute-sync!
  [computer]
  (loop [c computer]
    (if (:halted? c)
      c
      (recur (step! c :sync)))))

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
  (last (:output (<!! (last (map compute! amps))))))
