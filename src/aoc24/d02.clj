(ns aoc24.d02
  (:require [clojure.string :as s]
            [clojure.pprint :refer :all]
            [aoc24.utils :refer :all]))

(def ex
  (get-line-num
    "7 6 4 2 1
    1 2 7 8 9
    9 7 6 2 1
    1 3 2 4 5
    8 6 4 4 1
    1 3 6 7 9"))

(def in (get-line-num))

(defn safe?[l]
  (let [nl (sort l)
        inc? (< (nth l 0) (nth l 1))
        c1 (if inc?
             (= nl l)
             (= l (reverse nl)))]
    (when c1
      (->> (map #(- %2 %1) nl (next nl))
           (every? #(< 0 % 4))
           ;;(#(do (prn %) %))
           ))))


(defn safe2?[l]
  (let [ll (map #(concat (take %1 l)
                         (drop (inc %1) l))
                (range (count l)))]
    (some safe? ll)))


(defn q1 [ll]
  (reduce (fn [acc l]
            (if (safe? l) (inc acc) acc))
          0 ll))

#_(q1 in)

(defn q2 [ll]
  (reduce (fn [acc l]
            (if (safe2? l) (inc acc) acc))
          0 ll))


#_(q2 in)
