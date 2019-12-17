(ns aoc.2019.day14
  (:require
   [clojure.string :as str]
   [clojure.pprint :as pprint]
   [aoc.utils :as u]))

(defn parse-ingredient [[_ amt chem]]
  [chem (read-string amt)])

(defn parse-reaction [s]
  (let [[inputs [[out-chem amt]]] (->> (str/split s #"=>")
                                       (map #(re-seq #"(\d+) ([A-Z]+)" %))
                                       (map #(map parse-ingredient %)))]
    {out-chem (assoc (into {} inputs) out-chem (- amt))}))

(defn parse-reactions [s]
  (into {} (mapcat parse-reaction (str/split-lines s))))

(defn ingredients-for-fuel [reactions desired-fuel]
  (loop [desired (-> (zipmap (keys reactions) (repeat 0N))
                     (assoc "FUEL" desired-fuel))]
    (if-let [[chem amt] (u/get-some #(pos? (val %)) (dissoc desired "ORE"))]
      (let [reaction (reactions chem)
            n-orders (Math/ceil (/ amt (- (reaction chem))))]
        (recur (reduce
                (fn [desired [ingredient order-sz]]
                  (update desired ingredient (fnil + 0N) (* n-orders order-sz)))
                desired
                reaction)))
      desired)))

(defn ore-for-fuel [reactions desired-fuel]
  (-> (ingredients-for-fuel reactions desired-fuel)
      (get "ORE")))

;; part 1
(def reactions (parse-reactions (u/input 2019 14)))
(ore-for-fuel reactions 1)

;; part 2
(defn binary-search [f goal low high]
  (loop [low low
         high high
         guesses []]
    (if (>= high low)
      (let [mid (quot (+ low high) 2)
            res (f mid)]
        (cond
          (< res goal) (recur (inc mid) high (conj guesses {:guess mid :res res}))
          (> res goal) (recur low (dec mid) guesses)
          :else {:res mid}))
      {:guesses guesses :res nil})))

(binary-search (partial ore-for-fuel reactions) 1e12 1 1e12)
