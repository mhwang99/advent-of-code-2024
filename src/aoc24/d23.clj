(ns aoc24.d23
  (:require [clojure.set :as set]
            [clojure.string :as s]
            [clojure.pprint :refer :all]
            [aoc24.utils :refer :all]))

(defn parse-input [s]
  (reduce
    (fn [m [s e]]
      (-> m
          (update (s/trim s) (fnil conj #{}) (s/trim e))
          (update (s/trim e) (fnil conj #{}) (s/trim s))))
    {} (get-line-split s "-")))

(def in (parse-input (get-res)))

(defn is-group?
  [m group]
  (every?
    #(every?
       (fn [other] ((m other) %))
       (disj group %))
    group))

(defn next-groups
  [m group]
  (let [linked (reduce set/union (map m group))]
    (->> (set/difference linked group)
         (map #(conj group %)))))

(defn find-groups
  ([m]
   (find-groups m -1))
  ([m max-cnt]
   (loop [groups (map hash-set (keys m))]
     (if (or (< (count groups) 2)
             (and (> max-cnt 0)
                  (>= (count (first groups)) max-cnt)))
       groups
       (->> groups
            (mapcat #(next-groups m %))
            (filter #(is-group? m %))
            set recur)))))

(defn q1
  [m]
  (->> (find-groups m 3)
       (filter (fn [group] (some #(= \t (first %)) group)))
       count))

#_(q1 in)

(defn q2
  [m]
  (->> (find-groups m)
       first sort
       (s/join ",")))

#_(q2 in)
