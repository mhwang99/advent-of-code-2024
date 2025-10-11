(ns aoc24.d24
  (:require [clojure.set :as set]
            [clojure.string :as s]
            [clojure.pprint :refer :all]
            [aoc24.utils :refer :all]))

(defn parse-input [s]
  (let [sl (get-lines s)
        m (->> (take-while (comp seq s/trim) sl)
               (map #(let [[k v] (s/split % #":")]
                       (let [v (atoi (s/trim v))]
                         [(s/trim k) v])))
               (into {}))
        wires (->> (drop (inc (count m)) sl)
                   (map #(let [[a op b _ k] (s/split % #" ")]
                           [(s/trim k) [(s/trim op) (s/trim a) (s/trim b)]]))
                   (into {}))]
    [m wires]))

(def in (parse-input (get-res)))

(defn get-value
  [m wires k]
  (if-let [v (get m k)]
    [m v]
    (when-let [[op a b] (get wires k)]
      (let [[m av] (get-value m wires a)
            [m bv] (get-value m wires b)
            op (case op "OR" bit-or "XOR" bit-xor "AND" bit-and)
            v (op av bv)]
        [(assoc m k v) v]))))

(defn get-all-value
  [[m wires] c]
  (loop [i 0
         m m
         ret 0]
    (if-let [[m v] (get-value m wires (format "%s%02d" c i))]
      (recur (inc i) m (+ ret (bit-shift-left v i)))
      ret)))

(defn q1
  [mw]
  (get-all-value mw \z))

#_(q1 in)

(defn xk [i] (format "x%02d" i))
(defn yk [i] (format "y%02d" i))
(defn zk [i] (format "z%02d" i))

(defn gen-map
  ([]
   (->> (range 45)
        (mapcat (fn [i] [[(xk i) 0] [(yk i) 0]]))
        (into {})))
  ([i x y c]
   (cond-> (gen-map)
     true (assoc (xk i) x
                 (yk i) y)
     (pos? i) (assoc (xk (dec i)) c
                     (yk (dec i)) c))))

(defn get-linked
  [wires k]
  (loop [ks #{k}
         ret #{k}]
    (if (empty? ks)
      ret
      (let [nks (set
                  (mapcat
                    #(when-let [[op a b] (get wires %)]
                       [a b])
                    ks))]
        (recur nks (into ret nks))))))

(defn wire-groups
  [wires]
  (let [sw (reduce
             (fn [sw k]
               (let [linked (get-linked wires k)]
                 (assoc sw k linked)))
             {} (keys wires))
        [wg] (reduce
               (fn [[wg sw] i]
                 (let [xs (keep (fn [[k es]] (when (es (xk i)) k)) sw)
                       links (remove (fn [[k]] (#{\x \y} k)) xs)]
                   [(assoc wg i links)
                    (reduce dissoc sw xs)]))
               [{} sw] (reverse (range 45)))]
    [sw wg]))

(defn check-wire
  [wires i]
  (->> (for [x [0 1] y [0 1] c (if (pos? i) [0 1] [0])] [x y c])
       (every? (fn [[x y c]]
                 (= (second (get-value (gen-map i x y c) wires (zk i)))
                    (bit-xor c (bit-xor x y)))))))

(defn get-combos [n l]
  (->> (reduce
         (fn [acc i]
           (->> (mapv #(conj % i) acc)
                (into acc)
                (filterv #(<= (count %) n))))
         [[]] l)
       (filterv #(= (count %) n))))

(defn find-switches
  [sub-wires groups wires]
  (reduce
    (fn [[wires swaps :as ws] i]
      (if (check-wire wires i)
        ws
        (->> (get groups i)
             (get-combos 2)
             (some
               (fn [[e1 e2]]
                 (when-not (or ((sub-wires e1) e2)
                               ((sub-wires e2) e1))
                   (let [wires (assoc wires
                                      e1 (get wires e2)
                                      e2 (get wires e1))]
                     (when (check-wire wires i)
                       [wires (conj swaps e1 e2)]))))))))
    [wires #{}]
    (range 45)))

(defn q2
  [[_ wires]]
  (let [[sub-wires wg] (wire-groups wires)]
    (->> (find-switches sub-wires wg wires)
         second sort
         (s/join ","))))

#_(q2 in)
