(ns aoc24.d06
  (:require [clojure.string :as s]
            [clojure.pprint :refer :all]
            [aoc24.utils :refer :all]))


(def ex
  (get-line-chars
"....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#..."))
(def in (get-line-chars))


(defn get-guard [board]
  (let [rows (count board)
        cols (count (first board))]
    (reduce
      (fn [_ row]
        (when-let [col (reduce
                         (fn [_ col]
                           (when (= \^ (get-in board [row col]))
                             (reduced col)))
                         nil (range cols))]
          (reduced [row col])))
      nil (range rows))))

(defn get-next
  [pt d]
  (->> d
       (get {:u [-1 0] :d [1 0] :r [0 1] :l [0 -1]})
       (mapv + pt)))

(defn turn [d]
  (get {:u :r, :r :d, :d :l, :l :u} d))


(defn q1
  [board]
  (loop [dir :u
         pt (get-guard board)
         path #{pt}]
    (let [npt (get-next pt dir)
          v (get-in board npt)]
      (cond
        (nil? v) (count path)
        (= v \#) (recur (turn dir) pt path)
        :else (recur dir npt (conj path npt))))))

#_(q1 in)

(defn loop?
  [board guard-pt]
  (loop [dir :u
         pt guard-pt
         path #{}]
    (let [npt (get-next pt dir)
          v (get-in board npt)]
      (cond
        (nil? v) false
        (get path [pt dir]) true
        (= v \#) (recur (turn dir) pt (conj path [pt dir]))
        :else (recur dir npt (conj path [pt dir]))))))

(defn q2
  [board]
  (let [all-pts (get-all-points board)
        guard-pt (get-guard board)]
    (reduce (fn [acc block]
              (let [v (get-in board block)]
                (if (or (= v \#) (= v \^))
                  acc
                  (if (loop? (assoc-in board block \#) guard-pt)
                    (inc acc)
                    acc))))
            0 all-pts)))

#_(q2 in)




