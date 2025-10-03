(ns aoc24.d16
  (:require [clojure.set :as set]
            [clojure.string :as s]
            [clojure.pprint :refer :all]
            [aoc24.utils :refer :all]))

(def ex (get-line-chars
"###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############"))
(def in (get-line-chars))

(def dir-map
  {:r [0 1]
   :l [0 -1]
   :u [-1 0]
   :d [1 0]})

(def turn-map
  {:r [:u :d]
   :l [:u :d]
   :u [:l :r]
   :d [:l :r]})

(defn get-next
  [[pt d s]]
  (let [new-pt (mapv + pt (dir-map d))
        [dir1 dir2] (turn-map d)]
    [[new-pt d (inc s)]
     [pt dir1 (+ s 1000)]
     [pt dir2 (+ s 1000)]]))

(defn find-route
  [board]
  (let [spt (find-x board \S)]
    (loop [ptds [[spt :r 0]]
           path-map {}]
      (if-not (seq ptds)
        path-map
        (let [[pt dir score :as ptd] (first ptds)
              old-score (get path-map [pt dir])
              v (get-in board pt)
              remain (vec (next ptds))]
          (cond
            (and old-score (<= old-score score)) (recur remain path-map)
            (= \# v) (recur remain path-map)
            (= \E v) (recur remain
                            (assoc path-map [pt dir] score))
            :else (recur (into remain (get-next ptd))
                         (assoc path-map [pt dir] score))))))))

(defn q1
  [board]
  (let[path-map (find-route board)
       ept (find-x board \E)
       scores (keep #(get path-map [ept %])
                    [:r :l :u :d])]
    (if (seq scores)
      (->> scores
           (apply min))
      nil)))

#_(q1 in)

(defn get-next2
  [board [pt d s his]]
  (let [new-pt (mapv + pt (dir-map d))]
    (reduce (fn [acc dir]
              (if (->> (mapv + pt (dir-map dir))
                       (get-in board)
                       (= \#))
                acc
                (conj acc [pt dir (+ s 1000) his])))
            [[new-pt d (inc s) (conj his new-pt)]]
            (turn-map d))))

(defn find-route2
  [board]
  (let [spt (find-x board \S)]
    (loop [ptds [[spt :r 0 #{spt}]]
           path-map {}
           ret nil]
      (if-not (seq ptds)
        ret
        (let [[pt dir score his :as ptd] (first ptds)
              old-score (get path-map [pt dir])
              v (get-in board pt)
              remain (next ptds)]
          (cond
            (and old-score (< old-score score)) (recur remain path-map ret)
            (= \# v) (recur remain (assoc path-map [pt dir] -1) ret)
            (= \E v) (let [[r-score r-his] ret
                           new-ret (cond
                                     (nil? r-score) [score his]
                                     (< score r-score) [score his]
                                     (> score r-score) ret
                                     :else [score (into his r-his)])]
                       (recur remain path-map new-ret))
            :else (recur (into remain (get-next2 board ptd))
                         (assoc path-map [pt dir] score)
                         ret)))))))

(defn q2
  [board]
  (-> board find-route2 second count))

#_(q2 in)
