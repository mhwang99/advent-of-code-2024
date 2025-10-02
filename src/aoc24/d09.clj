(ns aoc24.d09
  (:require [clojure.string :as s]
            [clojure.pprint :refer :all]
            [aoc24.utils :refer :all]))

(def ex
  (mapv atoi (seq
"2333133121414131402")))
(def in (mapv atoi (seq (get-res))))

(defn make-file
  [size-list]
  (vec
    (mapcat (fn [idx size]
              (if (even? idx)
                (repeat size (/ idx 2))
                (repeat size nil)))
            (range) size-list)))


(defn compress-file
  [file]
  (let [[nl dl] (reduce
                  (fn [[nl dl] i]
                    (if (nil? (get file i))
                      [(conj nl i) dl]
                      [nl (conj dl i)]))
                  [[] []] (range (count file)))]
    (loop [nl nl
           dl (reverse dl)
           file file]
      (let [ni (first nl)
            di (first dl)]
        (if (> ni di)
          file
          (recur (next nl) (next dl)
                 (-> file
                     (assoc ni (get file di))
                     (assoc di nil))))))))

(defn check-sum
  [file]
  (->> file
       (map-indexed (fn [i v] (if v (* i v) 0)))
       (apply +)))

(defn q1
  [size-list]
  (->> size-list
       make-file
       compress-file
       check-sum))

#_(q1 in)

(defn make-file
  [size-list]
  (vec
    (mapcat (fn [idx size]
              (if (even? idx)
                (repeat size (/ idx 2))
                (repeat size nil)))
            (range) size-list)))

; nl : nil list : [[start-index len]]
; dm : data map : {value [start-index len]}
(defn get-data-info
  [size-list]
  (loop [idx 0
         fidx 0
         sl size-list
         nl []
         dm {}
         mx -1]
    (if (seq sl)
      (let [size (first sl)]
        (if (even? idx)
          (recur (inc idx) (+ fidx size) (next sl)
                 nl
                 (assoc dm (/ idx 2) [fidx size])
                 (/ idx 2))
          (recur (inc idx) (+ fidx size) (next sl)
                 (if (> size 0) (conj nl [fidx size]) nl)
                 dm
                 mx)))
      [nl dm mx])))

(defn get-matching-ni
  [nl dstart dlen]
  (reduce (fn [_ i]
            (let [[nstart nlen] (get nl i)]
              (if (> nstart dstart)
                (reduced nil)
                (if (>= nlen dlen)
                  (reduced i)))))
          nil (range (count nl))))

(defn draw-file
  [file start len v]
  (reduce (fn [acc i] (assoc acc i v))
          file (range start (+ start len))))

(defn compress2-file
  [size-list file]
  (let [[nl dm mx] (get-data-info size-list)]
    (loop [nl nl
           v mx
           file file]
      (if (< v 0)
        file
        (let [[dstart dlen]  (get dm v)]
          (if (< dlen 1)
            (recur nl (dec v) file)
            (let [ni (get-matching-ni nl dstart dlen)]
              (if (nil? ni)
                (recur nl (dec v) file)
                (let [[nstart nlen] (get nl ni)]
                  (recur (assoc nl ni [(+ nstart dlen) (- nlen dlen)])
                         (dec v)
                         (-> file
                             (draw-file nstart dlen v)
                             (draw-file dstart dlen nil))))))))))))


(defn q2
  [size-list]
  (->> size-list
       make-file
       (compress2-file size-list)
       check-sum))

#_(q2 in)

