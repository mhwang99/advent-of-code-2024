(ns aoc24.d20
  (:require [clojure.set :as set]
            [clojure.string :as s]
            [clojure.pprint :refer :all]
            [aoc24.utils :refer :all]))

(defn parse-input
  [s]
  (let [board (get-line-chars s)
        start (find-x board \S)
        end (find-x board \E)
        board (-> board
                  (assoc-in start \.)
                  (assoc-in end \.))]
    [board start end]))

(def in (parse-input (get-res)))

(defn get-distance-map
  [board start]
  (loop [pts #{start}
         dist-map {start 0}
         i 1]
    (if (empty? pts)
      dist-map
      (let [npts (->> (mapcat next-points pts)
                      (filter #(and (not (contains? dist-map %))
                                    (= \. (get-in board %))))
                      set)]
        (recur npts (->> (map vector npts (repeat i)) (into dist-map)) (inc i))))))

(defn get-cheat-nexts
  [board [row col] max-time]
  [max-time]
  (->> (for [x (range (inc max-time))
             y (range (inc max-time))
             :when (<= 2 (+ x y) max-time)]
         (let [l (+ x y)]
           [[[(+ row y) (+ col x)] l]
            [[(- row y) (+ col x)] l]
            [[(+ row y) (- col x)] l]
            [[(- row y) (- col x)] l]]))
       (mapcat identity) set
       (filter #(= \. (get-in board (first %))))
       (into {})))

(defn get-cheat-map
  [board cheat-time dist-map]
  (reduce
    (fn [cheat-map spt]
      (let [spt-dis (dist-map spt)]
        (reduce
          (fn [cheat-map [ept ctime]]
            (let [diff (- spt-dis (dist-map ept) ctime)]
              (if (pos? diff)
                (assoc cheat-map [spt ept] diff)
                cheat-map)))
          cheat-map (get-cheat-nexts board spt cheat-time))))
    {} (keys dist-map)))

(defn q1
  [[board start end] least]
  (->> (get-distance-map board end)
       (get-cheat-map board 2)
       vals
       (filter #(>= % least))
       count))

(defn q2
  [[board start end] least]
  (->> (get-distance-map board end)
       (get-cheat-map board 20)
       vals
       (filter #(>= % least))
       count))

#_(q1 in 100)
#_(q2 in 100)
