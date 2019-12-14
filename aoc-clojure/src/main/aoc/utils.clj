(ns aoc.utils
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn input
  [season day]
  (-> (format "%s/day%02d" season day)
      io/resource
      slurp
      str/trim))

(defn manhattan
  "Calculates the manhattan distance between points, or from the origin. Works
   for points of any dimensionality."
  ([pt] (manhattan pt (repeat (count pt) 0)))
  ([pta ptb]
   (assert (= (count pta) (count ptb))
           "Points must have same dimensions to calculate manhattan distance.")
   (reduce + (map #(Math/abs ^Integer %) (map - pta ptb)))))

(defn ->int [s]
  (when s (Integer/parseInt s)))

(defn get-digit
  "Gets the ith digit of n. Works for decimal digits as well.

  (digit 1234 0) => 4
  (digit 1234.5 -1) => 5"
  [n i]
  (int (rem (/ (bigdec n)
               (bigdec (Math/pow 10 i)))
            10)))

(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn lcm [nums]
  (letfn [(increment-least-by [a b]
            (let [idx (.indexOf a (apply min a))]
              (update a idx + (get b idx))))]
    (loop [n nums]
      (if (apply = n)
        (first n)
        (recur (increment-least-by n nums))))))
