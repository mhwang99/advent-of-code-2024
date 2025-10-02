(ns aoc24.d11
  (:require [clojure.string :as s]
            [clojure.pprint :refer :all]
            [aoc24.utils :refer :all]))

(def ex (first (get-line-num "125 17")))
(def in (first (get-line-num)))

(defn apply-rule
  [l]
  (doall
    (mapcat (fn [e]
              (if (= e 0)
                [1]
                (let [s (str e)]
                  (if (even? (count s))
                    (let [n (/ (count s) 2)]
                      [(atoi (apply str (take n s)))
                       (atoi (apply str (drop n s)))])
                    [(* 2024 e)]))))
            l)))

(def get-stone-count
  (memoize
    (fn [e n]
      (let [r (apply-rule (if (number? e) [e] e))]
        (if (< n 2)
          (count r)
          (->> r
               (map #(get-stone-count % (dec n)))
               (apply +)))))))


(defn q1 [l]
  (get-stone-count l 25))


(defn q2 [l]
  (get-stone-count l 75))

#_(q1 in)
#_(q2 in)



