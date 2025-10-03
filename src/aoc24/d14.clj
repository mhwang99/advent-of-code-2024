(ns aoc24.d14
  (:require [clojure.set :as set]
            [clojure.string :as s]
            [clojure.pprint :refer :all]
            [aoc24.utils :refer :all]))

(def ex (get-line-num
"p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3"))
(def in (get-line-num))

(defn move-robots
  [[wide tall] robots]
  (mapv (fn [[x y vx vy]]
          [(mod (+ x vx) wide)
           (mod (+ y vy) tall)
           vx vy])
        robots))

(defn get-safety
  [[wide tall] robots]
  (let [midx (quot wide 2)
        midy (quot tall 2)]
    (->> robots
         (group-by (fn [[x y _ _]]
                     [(compare x midx)
                      (compare y midy)]))
         (keep (fn [[[cx cy] l]]
                 (when-not (or (= 0 cx) (= 0 cy))
                   (count l)))))))

(defn q1
  [board robots]
  (->> robots
       (iterate #(move-robots board %))
       (#(nth % 100))
       (get-safety board)
       (apply *)))

#_(q1 [11 7] ex)
#_(q1 [101 103] in)

(defn print-robots
  [[wide tall] robots]
  (doseq [y (range tall)]
    (doseq [x (range wide)]
      (print
        (if (some #(= [x y] (take 2 %)) robots)
          \# \.)))
    (println)))

(defn q2
  [board robots]
  (loop [n 1
         robots robots]
    (let [nr (move-robots board robots)
          pts (set (map #(take 2 %) nr))]
      (if (= (count pts) (count robots))
        (do
          (print-robots board nr)
          n)
        (recur (inc n) nr)))))

#_(q2 [101 103] in)
