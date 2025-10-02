(ns aoc24.d12
  (:require [clojure.set :as set]
            [clojure.string :as s]
            [clojure.pprint :refer :all]
            [aoc24.utils :refer :all]))

(def ex (get-line-chars
"RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE"))
(def in (get-line-chars))

(defn get-area
  [board pt]
  (let [v (get-in board pt)]
    (loop [cur #{pt}
           area #{pt}]
      (if (seq cur)
        (let [ncur (->> cur
                        (mapcat next-points)
                        (filter #(and (not (area %)) (= v (get-in board %)))))]
          (recur (set ncur) (into area ncur)))
        area))))

(defn get-areas
  [board]
  (loop [pts (set (get-all-points board))
         areas #{}]
    (if (seq pts)
      (let [area (get-area board (first pts))]
        (recur (set/difference pts area) (conj areas area)))
      areas)))

(defn get-perimeter
  [area]
  (reduce (fn [cnt pt]
            (->> (next-points pt)
                 (remove area)
                 count (+ cnt)))
          0 area))

(defn q2
  [board]
  (->> (get-areas board)
       (map #(* (count %) (get-perimeter %)))
       (apply +)))

#_(q1 in)

(defn remove-same-side
  [pms [loc pt :as v]]
  (reduce
    (fn [pms offset]
      (loop [last-pt pt
             pms pms]
        (let [next-pt (mapv + last-pt offset)]
          (if-let [v (pms [loc next-pt])]
            (recur next-pt (disj pms v))
            pms))))
    (disj pms v) (-> loc get-direction get-offsets)))

(defn get-side-count
  [area]
  (let [pms (reduce (fn [acc pt]
                      (->> (next-points pt :all)
                           (map vector [:up :down :right :left])
                           (remove (comp area second))
                           (into acc)))
                    #{} area)]
    (loop [pms pms
           ret #{}]
      (if (seq pms)
        (let [v (first pms)]
          (recur (remove-same-side pms v) (conj ret v)))
        (count ret)))))

(defn q2
  [board]
  (->> (get-areas board)
       (map #(* (count %) (get-side-count %)))
       (apply +)))


#_(q2 in)
