(ns aoc24.d21
  (:require [clojure.set :as set]
            [clojure.string :as s]
            [clojure.pprint :refer :all]
            [aoc24.utils :refer :all]))

(def in (get-lines))

(defn key-map
  [board]
  (reduce
    (fn [m r]
      (reduce
        (fn [m c]
          (if-let [v (get-in board [r c])]
            (assoc m v [r c])
            m))
        m (range (count (first board)))))
    {} (range (count board))))

(def dirpad-board [[nil \U \A] [\L \D \R]])
(def numpad-board [[\7 \8 \9] [\4 \5 \6] [\1 \2 \3] [nil \0 \A]])
(def dboard-keymap (key-map dirpad-board))
(def nboard-keymap (key-map numpad-board))

(def find-next-codes
  (memoize
    (fn [code-type sv ev]
      (let [board (if (= :dir code-type) dirpad-board numpad-board)
            keymap (if (= :dir code-type) dboard-keymap nboard-keymap)
            spt (get keymap sv)
            ept (get keymap ev)]
        (loop [ptps #{[spt []]}
               seen #{}]
          (let [ret (keep #(when (= (first %) ept)
                             (->> (conj (second %) \A) (apply str)))
                          ptps)]
            (cond
              (seq ret) ret
              (empty? ptps) nil
              :else (let [nptps (->> ptps
                                     (mapcat (fn [[pt path]]
                                               (map (fn [npt d] [npt (conj path d)])
                                                    (next-points pt)
                                                    [\D \U \L \R])))
                                     (remove #(let [pt (first %)]
                                                (or (seen pt)
                                                    (nil? (get-in board pt))))))]
                      (recur nptps (into seen (map first nptps)))))))))))

(defn get-next-codes
  [code-type code]
  (->> (str "A" code)
       (partition 2 1)
       (mapv (fn [[sv ev]] (find-next-codes code-type sv ev)))))

(def get-nth-code-len
  (memoize
    (fn [code-type code n]
      (if (< n 0)
        (count code)
        (->> (get-next-codes code-type code)
             (map (fn [codes]
                    (->> (map #(get-nth-code-len :dir % (dec n)) codes)
                         (reduce min))))
             (reduce +))))))

(defn q1
  [codes]
  (reduce
    (fn [ret code]
      (let [cnt (get-nth-code-len :num code 2)
            coden (->> code drop-last (apply str) atoi)]
        (+ ret (* cnt coden))))
    0 codes))

#_(q1 in)

(defn q2
  [codes]
  (reduce
    (fn [ret code]
      (let [cnt (get-nth-code-len :num code 25)
            coden (->> code drop-last (apply str) atoi)]
        (+ ret (* cnt coden))))
    0 codes))

#_(q2 in)
