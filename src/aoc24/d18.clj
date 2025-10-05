(ns aoc24.d18
  (:require [clojure.set :as set]
            [clojure.string :as s]
            [clojure.pprint :refer :all]
            [aoc24.utils :refer :all]))

(def in (get-line-num))

(defn make-board
  [rows cols]
  (vec (repeat rows (vec (repeat cols \.)))))

(defn find-route
  [board start dest]
  (loop [pts #{start}
         paths #{}
         i 0]
    (cond
      (pts dest) i
      (empty? pts) nil
      :else (let [npts (->> (mapcat next-points pts)
                            (filter #(and (not (paths %))
                                          (= \. (get-in board %))))
                            set)]
              (recur npts (into paths npts) (inc i))))))

(defn q1
  [pts [dr dc :as dest] n]
  (let [board (make-board (inc dr) (inc dc))
        board (reduce (fn [board pt]
                        (assoc-in board pt \#))
                      board (take n pts))]
    (find-route board [0 0] dest)))

#_(q1 in [70 70] 1024)

(defn q2
  [pts [dr dc :as dest] n]
  (let [board (make-board (inc dr) (inc dc))
        board (reduce (fn [board pt]
                        (assoc-in board pt \#))
                      board (take n pts))]
    (reduce (fn [board pt]
              (let [board (assoc-in board pt \#)]
                (if (nil? (find-route board [0 0] dest))
                  (reduced (s/join "," pt))
                  board)))
            board (drop n pts))))

#_(q2 in [70 70] 1024)
