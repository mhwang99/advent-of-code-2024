(ns aoc24.d15
  (:require [clojure.set :as set]
            [clojure.string :as s]
            [clojure.pprint :refer :all]
            [aoc24.utils :refer :all]))

(defn parse-input [s]
  (let [ll (get-line-chars s)]
    [(vec (take-while seq ll))
     (flatten (drop-while seq ll))]))

(def ex (parse-input
"##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^"))
(def in (parse-input (get-res)))

(def move-map
  {\< [0 -1]
   \> [0 1]
   \^ [-1 0]
   \v [1 0]})

(defn find-pushing
  [move pts vs]
  (if (or (= \< move) (= \> move))
    pts
    (->> (mapcat (fn [pt v]
                   (case v
                     \O [pt]
                     \[ [pt (mapv + pt [0 1])]
                     \] [(mapv + pt [0 -1]) pt]
                     nil))
                 pts vs)
         set vec)))

(defn move-robots
  [board rpt move]
  (let [offset (get move-map move)
        new-rpt (mapv + rpt offset)]
    (loop [pts [new-rpt]
           path [rpt]]
      (let [vs (map #(get-in board %) pts)]
        (cond
          (some #(= \# %) vs) [board rpt]
          (not-every? #(= \. %) vs) (let [pts (find-pushing move pts vs)]
                                      (recur (map #(mapv + % offset) pts) (into path pts)))
          :else [(reduce
                   (fn [board pt]
                     (-> board
                         (assoc-in (mapv + pt offset) (get-in board pt))
                         (assoc-in pt \.)))
                   board (reverse path))
                 new-rpt])))))

(defn calc-board
  [board]
  (->> board
       (map-indexed
         (fn [row line]
           (map-indexed
             (fn [col v]
               (if (or (= \O v) (= \[ v))
                 (+ (* row 100) col)
                 0))
             line)))
       flatten
       (apply +)))

(defn q1
  [[board moves]]
  (->> moves
       (reduce (fn [[board rpt] move]
                 (move-robots board rpt move))
               [board (find-x board \@)])
       first calc-board))

#_(q1 ex)
#_(q1 in)

(defn enlarge-board
  [board]
  (->> board
       (mapv (fn [l]
               (vec (mapcat #(case %
                               \# "##"
                               \O "[]"
                               \. ".."
                               \@ "@.")
                            l))))))

(defn q2
  [[board moves]]
  (q1 [(enlarge-board board) moves]))

#_(q2 ex)
#_(q2 in)

