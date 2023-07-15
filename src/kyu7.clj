(ns kyu7
  (:require [clojure.string :as str]
            [clojure.string :as string]))

;https://www.codewars.com/kata/5949481f86420f59480000e7
(defn odd-or-even [xs]
  ; TODO
  (if (odd? (apply + xs)) "odd" "even"))

;https://www.codewars.com/kata/540c33513b6532cd58000259
(defn sum
  [& args]
  (apply + args))

;https://www.codewars.com/kata/567bed99ee3451292c000025
(defn vowel[str]
  (contains? #{"a" "e"  "i"  "o"  "u"} (string/lower-case str)))


;https://www.codewars.com/kata/5a023c426975981341000014
(defn third-angle [a b]
  (- 180 (+ a b)))

;https://www.codewars.com/kata/57b68bc7b69bfc8209000307
(defn average [data]
  (int (/ (apply + data) (count data))))

;https://www.codewars.com/kata/5966eeb31b229e44eb00007a
(defn vaporcode [s]
  (->> s
       seq
       (map #(str %))
       (filter #(not= " " %))
       (map #(str/upper-case %))
       (str/join #"  ")))

;https://www.codewars.com/kata/51f2b4448cadf20ed0000386
(defn remove-url-anchor [url]
  (first (str/split url #"#")))

;https://www.codewars.com/users/Enligth1337/completed_solutions
(defn get-sum [a b]
  (let [x (min a b)
        y (max a b)]
    (apply + (range x (inc y)))))

;https://www.codewars.com/kata/554b4ac871d6813a03000035
(defn high-and-low [s]
  (let [coll (map read-string (str/split s #"\s"))
        x (apply max coll)
        y (apply min coll)]
    (str x " " y)))

;https://www.codewars.com/kata/56f699cd9400f5b7d8000b55
(defn fix-the-meerkat [arr]
  (reverse arr))

;https://www.codewars.com/users/Enligth1337/completed_solutions
(defn solution [n]
  (sort n))

;https://www.codewars.com/kata/5667e8f4e3f572a8f2000039
(defn accum [s]
  ; your code
  (as-> s n
        (map-indexed vector n)
        (reduce (fn [acc x]
                  (conj acc (apply str (.toUpperCase (str (second x)))
                                   (string/join ""
                                                (repeat (first x) (.toLowerCase(str (second x)))))))) [] n)
        (string/join "-" n)))


;https://www.codewars.com/kata/563b662a59afc2b5120000c6
(defn nb-year
  ([p0 percent aug p]
   (nb-year p0 percent aug p 1))
  ([p0 percent aug p years]
   (let [curr (+ p0 aug (int (* p0 (/ percent 100))))]
     (if (< curr p)
       (nb-year curr percent aug p (+ years 1))
       years ))))

;https://www.codewars.com/kata/56541980fa08ab47a0000040
(defn printer-error [s]
  ; your code
  (str (->> s
            seq
            (map str)
            (filter #(re-find #"[n-z]" %))
            count) "/" (count s)))

;https://www.codewars.com/kata/52fba66badcd10859f00097e
(def vowels #{"a" "e" "i" "o" "u" "A" "E" "I" "O" "U"})

(defn disemvowel
  [string]
  (as-> string n
        (str/split n #" ")
        (map #(seq %) n)
        (map #(remove (fn [x]
                        (if (contains? vowels (str x))
                          true
                          false))%) n)
        (map #(apply str %) n)
        (str/join " " n)))

;https://www.codewars.com/kata/5467e4d82edf8bbf40000155
(defn desc-order [n]
  (Integer/parseInt (str/join ""
                              (map str
                                   (sort-by -
                                            (map #(Integer/parseInt %)
                                                 (map str (seq (str n)))))))))

;https://www.codewars.com/kata/525e5a1cb735154b320002c8
(defn triangular [n]
  ;; TODO
  (if (and (< 0 n))
    (/ (* n (+ n 1)) 2)
    0))

;https://www.codewars.com/kata/5648b12ce68d9daa6b000099
(defn number
  [bus-stops]
  (apply + (map #(apply - %) bus-stops)))

(defn solve [s idx]
  (apply str
         (reduce (fn [acc x]
                   (if (and (>= x 0) (< x (count acc)))
                     (update acc x #(str/upper-case %))
                     acc))
                 (vec (map str (seq s)))
                 idx)))


(defn get-middle [s]
  (if (= 0 (rem (count s) 2))
    (str (get s (-(/ (count s) 2) 1)) (get s (/ (count s) 2)))
    (str (get s (int (/ (count s) 2))))))

;https://www.codewars.com/kata/51f2d1cafc9c0f745c00037d
(defn solution [strng ending]
  ;(->> strng
  ;      (take-last (count ending))
  ;      (map str )
  ;      (str/join "")
  ;      (= ending))
  (str/ends-with? strng ending))



(vaporcode "Why isn't my code working?")
(filter #(not= " " %) (map #(str %) (seq "Lets go to the movies")))
