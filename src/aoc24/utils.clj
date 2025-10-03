(ns aoc24.utils
  (:require [clojure.string :as s]))

(defn atoi [s] (Long. (str s)))

(defn get-res []
  (->> (s/split (str *ns*) #"\.")
       second
       (#(str "resources/" % ".txt"))
       slurp (apply str)))

(defn get-split
  ([]
   (get-split (get-res)))
  ([s]
   (get-split s " |,|\\n"))
  ([s delim]
   (s/split s (re-pattern delim))))

(defn get-lines
  ([]
   (get-lines (get-res)))
  ([s]
   (mapv s/trim (s/split s #"\n"))))

(defn get-line-chars
  ([]
   (get-line-chars (get-res)))
  ([s]
   (mapv (comp vec seq) (get-lines s))))

(defn get-line-ichars
  ([]
   (get-line-ichars (get-res)))
  ([s]
   (mapv #(mapv atoi %) (get-lines s))))

(defn get-line-split
  ([delim]
   (get-line-split (get-res) delim))
  ([s delim]
   (->> (s/split s #"\n")
        (mapv (fn [l]
                (s/split l (re-pattern delim)))))))

(defn get-line-word
  ([word]
   (get-line-word (get-res) word))
  ([s word]
   (->> (s/split s #"\n")
        (mapv (fn [l]
                (vec (re-seq (re-pattern word) l)))))))


(defn get-num
  ([]
   (get-num (get-res)))
  ([s]
   (get-num s false))
  ([s minus?]
   (->> s
        (re-seq (if minus? #"[+-]?\d+" #"\d+"))
        (mapv atoi))))

(defn get-line-num
  ([]
   (get-line-num (get-res)))
  ([s]
   (->> (s/split s #"\n")
        (mapv (fn [l]
                (->> (re-seq #"[+-]?\d+" l)
                     (mapv atoi)))))))

(defn reduce-walk
  [f acc form]
  (let [pf (partial reduce-walk f)]
    (if (coll? form)
      (f (reduce pf acc form) form)
      (f acc form))))

(defn find-x
  [board x]
  (let [rows (count board)
        cols (count (first board))]
    (loop [row 0]
      (if (= row rows)
        nil
        (if-let [pt (loop [col 0]
                      (cond
                        (= col cols) nil
                        (= x (get-in board [row col])) [row col]
                        :else (recur (inc col))))]
          pt
          (recur (inc row)))))))

(defn get-all-points
  [board]
  (let [rows (count board)
        cols (count (first board))]
    (mapcat #(mapv vector (repeat %1) %2)
            (range rows)
            (repeat (range cols)))))

(defn get-point-word
  [board pt offset-points]
  (let [cs (mapv #(get-in board (mapv + pt %)) offset-points)]
    (if (some nil? cs)
      nil
      (apply str cs))))

(defn get-dir-words
  [board pt n directions pred]
  (->> directions
       (map (fn [dir] (map #(map * dir (repeat %)) (range n))))
       (map #(get-point-word board pt %))
       (filter pred)))

(defn get-mid [l]
  (nth l (int (/ (count l) 2))))

(defn get-permutation
  [l cnt]
  (->> l
       (reduce (fn [ll x]
            (into ll (map #(conj % x)
                          (filter #(< (count %) cnt) ll)
                          )))
          [[]])
       (filter #(= (count %) cnt))))

(defn get-offsets [dir]
  (get {:hori [[0 1] [0 -1]]
        :vert [[1 0] [-1 0]]
        :all [[1 0] [-1 0] [0 -1] [0 1]]}
       dir))

(defn get-direction [loc]
  (get {:up :hori
        :down :hori
        :left :vert
        :right :vert}
       loc))

(defn next-points
  ([pt] (next-points pt :all))
  ([pt dir]
   (->> (get-offsets dir)
        (map #(mapv + pt %)))))

(defn print-board [board]
  (doseq [l board]
    (println (apply str l)))
  board)

