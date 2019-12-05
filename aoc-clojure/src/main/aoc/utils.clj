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

(defn manhattan [[^Integer x ^Integer y]]
  (+ (Math/abs x) (Math/abs y)))

(defn ->int [s]
  (when s (Integer/parseInt s)))
