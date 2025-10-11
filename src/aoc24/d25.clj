(ns aoc24.d25
  (:require [clojure.set :as set]
            [aoc24.utils :refer :all]))

(defn parse-input [s]
  (loop [l (get-line-chars s)
         ls []
         ks []]
    (if (seq l)
      (let [sl (vec (take-while seq l))
            l (drop (inc (count sl)) l)
            code (apply
                   map (fn [& cs] (->> cs (filter #(= \# %)) count dec))
                   sl)]
        (if (= \# (ffirst sl))
          (recur l (conj ls code) ks )
          (recur l ls (conj ks code))))
      [ls ks])))

(def in (parse-input (get-res)))

(defn q1
  [[ls ks]]
  (reduce
    (fn [cnt l]
      (reduce
        (fn [cnt k]
          (if (every? #(< % 6) (map + l k))
            (inc cnt) cnt))
        cnt ks)
      )
    0 ls))

#_(q1 in)
