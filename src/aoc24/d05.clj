(ns aoc24.d05
  (:require [clojure.string :as s]
            [clojure.pprint :refer :all]
            [aoc24.utils :refer :all]))


(def ex
"47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47")
(def in (get-res))

(defn parse-input
  [s]
  (loop [l (get-lines s)
         rules {}
         pages-list []]
    (if (seq l)
      (let [line (first l)]
        (if (seq line)
          (if-let [[_ a b :as c] (first (re-seq #"^(\d+)\|(\d+)$" line))]
            (recur (next l) (update rules (atoi b) (fnil conj #{}) (atoi a)) pages-list)
            (recur (next l) rules
                   (->> (s/split line #",")
                        (mapv atoi)
                        (conj pages-list))))
          (recur (next l) rules pages-list)))
      [rules pages-list])))


(defn check-rule
  [rules pages]
  (loop [l pages]
    (if-not (seq l)
      true
      (let [nxt (next l)]
        (if-let [rule (get rules (first l))]
          (when (not-any? rule nxt)
            (recur (next l)))
          (recur (next l)))))))

(defn q1
  [s]
  (let [[rules pages-list] (parse-input s)]
    (->> pages-list
         (filter #(check-rule rules %))
         (map get-mid) (apply +))))

#_(q1 in)

(defn q2
  [s]
  (let [[rules pages-list] (parse-input s)
        r-comp (fn [a b]
                 (if (get-in rules [a b])
                   1 -1))]
    (->> pages-list
         (remove #(check-rule rules %))
         (map #(sort r-comp %))
         (map get-mid)
         (apply +))))

#_(q2 in)

