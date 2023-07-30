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

;https://www.codewars.com/kata/51b6249c4612257ac0000005
(def numerals {:I 1 :V 5 :X 10 :L 50 :C 100 :D 500 :M 1000})

(defn translate-roman-numerals [roman]
  ;; your code here
  (->> roman
       seq
       (map str)
       (map keyword)
       (map numerals)
       (reduce (fn [acc t]
                 (if (> t (first acc))
                   (conj (rest acc) (- t (first acc)))
                   (conj acc t)
                   )) '(0))
       (apply +)
       ))

;https://www.codewars.com/kata/525f50e3b73515a6db000b83
(defn create-phone-number [nums]
  (let [coll (map #(apply str %) (partition-all  3 3 nums))]
    (str "(" (first coll) ") " (second coll) "-" (last (butlast coll)) (last coll))))

;https://www.codewars.com/kata/5a946d9fba1bb5135100007c
(defn prime [n]
  (if (= 2 (count (filter (fn [x]
                            (= 0 (rem n x))) (rest (range (inc n))))))
    n
    (recur (inc n))))


(defn minimum-number [numbers]
  ; TODO
  (let [sum (apply + numbers)]
    (- (prime sum) sum)))

;https://www.codewars.com/kata/54b42f9314d9229fd6000d9c
(defn encode-dups [text]
  ; Your brilliant code here :)
  (let [coll(seq (str/lower-case text))
        mp (frequencies coll)]
    (apply str (reduce (fn [acc x]
                         (if (> (get mp x) 1)
                           (conj acc ")")
                           (conj acc "("))) [] coll))))

;https://www.codewars.com/kata/55bf01e5a717a0d57e0000e
(defn persistence
  ([n]
   (persistence 0 n))
  ([step n]
   (if (= 1 (count (str n)))
     step
     (->> (str n)
          (re-seq #"\d")
          (map #(Integer/parseInt %))
          (apply *)
          (recur (inc step))))))

;https://www.codewars.com/kata/545cedaa9943f7fe7b000048
(defn pangram?
  [s]
  ; TODO
  (if (= 26 (count
              (set (re-seq #"[a-z]" (str/lower-case s)))))
    true
    false))

