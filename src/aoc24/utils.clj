(ns aoc24.utils
  (:require [clojure.string :as s]))

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

(defn atoi
  [s]
  (Integer. (str s)))

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

