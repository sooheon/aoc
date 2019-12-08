(ns aoc.2019.day08
  (:require [aoc.utils :as u]
            [clojure.string :as str]))

(def i (map u/->int (map str (u/input 2019 8))))

(def l (apply min-key
              #(count (filter zero? %))
              (partition (* 25 6) i)))

(* (count (filter #(= 1 %) l))
   (count (filter #(= 2 %) l)))
;; ans: 2032

(def colors [:black :white :transparent])

(def layers (partition (* 25 6) i))
(count layers)

(partition 25 (apply map
                     (fn [& pxs]
                       (case (first (drop-while #(= 2 %) pxs))
                         1 \#
                         0 \_))
                     layers))
