(ns kyu6)

;https://www.codewars.com/kata/5ce399e0047a45001c853c2b
(defn parts-sums [ls]
  (let [out '(0)
        coll (reverse ls)]
    (reduce (fn [acc x]
              (conj acc (+ x (first acc))))out coll)))

;https://www.codewars.com/kata/514b92a657cdc65150000006
(defn solution [number]
  (let [coll (range number)]
    (reduce (fn [acc x]
              (if (or (= (rem x 5) 0) (= (rem x 3) 0))
                (+ x acc) acc))
            0 coll)))

;https://www.codewars.com/kata/526571aae218b8ee490006f4
(defn count-bits [n]
  (Integer/bitCount n))

;https://www.codewars.com/kata/54da5a58ea159efa38000836
(defn find-odd [xs]
  (reduce bit-xor xs))

;https://www.codewars.com/kata/5264d2b162488dc400000001
(defn spin-words [strng]
  (as-> strng  n
        (str/split n #" ")
        (map #(if (<= 5 (count %))
                (str/reverse %)
                %) n)
        (str/join " " n)))

;https://www.codewars.com/kata/5420fc9bb5b2c7fd57000004
(defn highest-rank
  [data]
  (first (apply max-key val (frequencies data))))

;https://www.codewars.com/kata/541c8630095125aba6000c00
(defn digital-root [n]
  (let [curr (->> n
                  str
                  seq
                  (map #(read-string (str %)))
                  (apply +))]
    (if (< curr 10)
      curr
      (digital-root curr))))

;https://www.codewars.com/kata/523f5d21c841566fde000009
(defn array-diff [a b]
  (remove (fn [x]
            (contains? (set b) x)) a))

;https://www.codewars.com/kata/54bf1c2cd5b56cc47f0007a1
(defn duplicate-count [text]
  ; Happy coding!
  (->> text
       (str/lower-case)
       frequencies
       (filter #(> (second %) 1))
       count))

;https://www.codewars.com/kata/515de9ae9dcfc28eb6000001
(defn solution
  [s]
  (map #(apply str %) (partition 2 2 "_"  s)))





