(ns aoc24.d24
  (:require [clojure.set :as set]
            [clojure.string :as s]
            [clojure.pprint :refer :all]
            [aoc24.utils :refer :all]))

(defn parse-input [s]
  (let [sl (get-lines s)
        vm (->> (take-while (comp seq s/trim) sl)
                (map #(let [[k v] (s/split % #":")]
                        (let [v (atoi (s/trim v))]
                          [(s/trim k) ["AND" v v]])))
                (into {}))]
    (->> (drop (inc (count vm)) sl)
         (map #(let [[a op b _ k] (s/split % #" ")]
                 [(s/trim k) [(s/trim op) (s/trim a) (s/trim b)]]))
         (into vm))))

(def in (parse-input (get-res)))

(defn get-value
  ([m k]
   (get-value m k {}))
  ([m k swaps]
   (let [v (get m k)]
     (if (vector? v)
       (let [[op a b] v
             [m av] (if (string? a) (get-value m a swaps) [m a])
             [m bv] (if (string? b) (get-value m b swaps) [m b])
             op (case op "OR" bit-or "XOR" bit-xor "AND" bit-and)
             v (op av bv)
             m (assoc m k v)]
         (if-let [sk (get swaps k)]
           (let [[sm sv] (get-value m sk (dissoc swaps sk))]
             [(assoc sm k sv sk v) sv])
           [m v]))
       [m v]))))

(defn get-all-value
  [m c]
  (loop [i 0
         m m
         ret '()]
    (let [[m v] (get-value m (format "%s%02d" c i))]
      (if (nil? v)
        (->> (apply str ret)
             (#(Long/parseLong % 2)))
        (recur (inc i) m (conj ret v))))))

(defn q1
  [m]
  (get-all-value m \z))

#_(q1 in)

(defn get-z-expect [m]
  (let [x (get-all-value m \x)
        y (get-all-value m \y)
        z (->> (+ x y) Long/toBinaryString (map atoi) reverse vec)
        zkeys (->> (keys m) (filter #(= \z (first %))) sort)
        pad-len (- (count z) (count zkeys))
        z (nth (iterate #(conj % 0) z) pad-len)]
    (mapv vector zkeys z)))

(defn num-keys [m]
  (->> m (filter #(number? (second %))) (map first) set))

(defn get-combos [n l]
  (->> l
       (reduce
         (fn [acc i]
           (->> (map #(conj % i) acc)
                (into acc)
                (filter #(<= (count %) n))))
         [[]])
       (filter #(= (count %) n))))

(defn find-groups [m]
  (let [zcnt (->> (keys m) (filter #(= \z (first %))) count)
        nm (reduce
             (fn [acc k]
               (let [[km v] (get-value m k)
                     nks (num-keys km)]
                 (assoc acc k nks)))
             {} (keys m))
        [bm] (reduce
               (fn [[acc nm] i]
                 (let [xs (keep
                            (fn [[k es]]
                              (when (or (es (format "x%02d" i))
                                        (es (format "y%02d" i)))
                                k))
                            nm)
                       exs (remove (fn [[k]] (#{\x \y} k)) xs)]
                   [(assoc acc (format "z%02d" i) exs)
                    (reduce dissoc nm xs)]))
               [{} nm] (reverse (range zcnt)))]
    bm))

(defn find-switches
  [m groups expects swaps]
  (loop [m m
         expects expects
         swaps swaps]
      (if (empty? expects)
        [swaps]
        (let [[k ev] (first expects)
              [zm v] (get-value m k swaps)]
          (if (= v ev)
            (recur zm (next expects) swaps)
            (let [sl (->> (get groups k)
                          (get-combos 2)
                          (keep (fn [[e1 e2]]
                                  (let [sw (into swaps [[e1 e2] [e2 e1]])]
                                    (when (= ev (second (get-value m k sw)))
                                      sw)))))]
              (when (seq sl)
                (mapcat #(find-switches m groups expects %) sl))))))))

(defn q2
  [m]
  (let [expects (get-z-expect m)
        groups (find-groups m)]
    (->> (find-switches m groups expects {})
         first keys sort (s/join ","))))

;But, I can’t find all the values, so in the end, I have to use draw-groups to find them.
#_(q2 in)

(defn sort-ops
  [m l]
  (->> (map #(conj (get m %) %) l)
       (sort (fn [a b]
               (let [[oa aa ba] a
                     [ob ab bb] b]
                 (cond
                   (and (#{\x \y} (first aa))
                        (#{\x \y} (first ab))) (if (= oa "XOR") -1 1)
                   (#{\x \y} (first aa)) -1
                   (#{\x \y} (first aa)) 1
                   :else
                   (let [ao (case oa "AND" -1 "OR" 0 "XOR" 1)
                         bo (case oa "AND" -1 "OR" 0 "XOR" 1)]
                     (compare ao bo))))))
       vec))

(defn draw-groups
  [m file]
  (doseq [[k es] (sort (find-groups m))]
    (spit file (str k " ************************\n") :append true)
    (doseq [[op a b k] (sort-ops m es)]
      (spit file (str k " : " a " " op " "  b "\n") :append true))))

#_(draw-groups in "wires.txt")
