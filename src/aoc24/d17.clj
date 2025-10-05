(ns aoc24.d17
  (:require [clojure.set :as set]
            [clojure.string :as s]
            [clojure.pprint :refer :all]
            [aoc24.utils :refer :all]))

(defn parse-input
  [src]
  (let [[[a] [b] [c] _ prog] (get-line-num src)]
    {:A a
     :B b
     :C c
     :prog prog}))

(def in (parse-input (get-res)))

(defn get-combo
  [{:keys [A B C] :as com} x]
  (get {0 0, 1 1, 2 2, 3 3,
        4 A, 5 B, 6 C} x))

(defn adv
  [{:keys [A] :as com} x]
  (let [x (get-combo com x)]
    (assoc com :A (long (/ A (Math/pow 2 x))))))

(defn bxl
  [{:keys [B] :as com} x]
  (assoc com :B (bit-xor B x)))

(defn bst
  [{:keys [B] :as com} x]
  (let [x (get-combo com x)]
    (assoc com :B (mod x 8))))

(defn jnz
  [{:keys [A idx] :as com} x]
  (assoc com :idx (if (= A 0) idx x)))

(defn bxc
  [{:keys [B C] :as com} _]
  (assoc com :B (bit-xor B C)))

(defn out
  [com x]
  (let [x (get-combo com x)]
    (update com :out (fnil conj []) (mod x 8))))

(defn bdv
  [{:keys [A] :as com} x]
  (let [x (get-combo com x)]
    (assoc com :B (long (/ A (Math/pow 2 x))))))

(defn cdv
  [{:keys [A] :as com} x]
  (let [x (get-combo com x)]
    (assoc com :C (long (/ A (Math/pow 2 x))))))

(def ops [adv bxl bst jnz bxc out bdv cdv])

(defn run-computer
  ([com]
   (run-computer com -1))
  ([{:keys [prog] :as com} max-out]
   (let [cnt (count prog)]
     (loop [{:keys [idx out] :as com} (assoc com :idx 0 :out [])]
       (if (or (>= idx (dec cnt))
               (and (> max-out 0) (>= (count out) max-out)))
         out
         (let [[op x & _] (drop idx prog)
               com (update com :idx + 2)]
           (recur ((get ops op) com x))))))))

(defn q1
  [com]
  (s/join "," (run-computer com)))

#_(q1 in)

(defn get-possible-a
  [{:keys [prog] :as com}]
  (reduce (fn [acc i]
            (let [dest (take-last i prog)]
              (->> acc
                   (mapcat (fn[a] (map #(+ (* a 8) %) (range 8))))
                   (filter #(-> (assoc com :A %)
                                (run-computer i)
                                (= dest))))))
          [0] (range 1 (inc (count prog)))))

(defn q2
  [com]
  (apply min (get-possible-a com)))


#_(q2 in)
