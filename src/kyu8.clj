(ns kyu8)


(defn sum
  [a]
  (apply + a))

;https://www.codewars.com/kata/57a2013acf1fa5bfc4000921
(defn find-average
  [numbers]
  (/ (apply + numbers) (count numbers)))

;https://www.codewars.com/kata/53ee5429ba190077850011d4
(defn double-integer [i]
  (* i 2))

;https://www.codewars.com/kata/523b623152af8a30c6000027
(defn square [n]
  (* n n))

;https://www.codewars.com/kata/57f780909f7e8e3183000078
(defn grow [xs]
  (apply * xs))

;https://www.codewars.com/kata/544675c6f971f7399a000e79
(defn string-to-number [str]
  (Integer/parseInt str))

;https://www.codewars.com/kata/5265326f5fda8eb1160004c8
(defn number-to-string [num]
  (str num))

;https://www.codewars.com/kata/56dec885c54a926dcd001095
(defn opposite [number]
  (- number))

;https://www.codewars.com/kata/515e271a311df0350d00000f
(defn square-sum [lst]
  (->> lst
       (map #(* % %))
       (apply +)))

;https://www.codewars.com/kata/571d42206414b103dc0006a1
(defn arr [n]
  (range n))

;https://www.codewars.com/kata/53af2b8861023f1d88000832
(defn plays-banjo?
  [name]
  ; Implement me!
  (if
    (= (string/lower-case(first name)) "r")
    (str name " plays banjo" )
    (str name " does not play banjo")))

;https://www.codewars.com/kata/563a631f7cbbc236cf0000c2
(defn move [position roll]
  (+ position (* roll 2)))

;https://www.codewars.com/kata/557cd6882bfa3c8a9f0000c1
(defn how-old
  [her-old]
  (->> her-old
       (re-seq #"\d")
       (first)
       (Integer/parseInt)))


;https://www.codewars.com/kata/5715eaedb436cf5606000381
(defn positive-sum [x]
  (->> x
       (filter pos?)
       (apply +)))

;https://www.codewars.com/kata/5772da22b89313a4d50012f7
(defn greet [name_ owner]
  (if (= name_ owner) "Hello boss" "Hello guest"))

;https://www.codewars.com/kata/57f781872e3d8ca2a000007e
(defn maps [xs]
  ;(map #(+ % %) xs)
  (map #(* 2 %) xs))

;https://www.codewars.com/kata/55f73be6e12baaa5900000d4
(defn goals [la-liga-goals copa-del-rey-goals champions-league-goals]
  (+ la-liga-goals copa-del-rey-goals champions-league-goals))

;https://www.codewars.com/kata/57a0e5c372292dd76d000d7e
(defn repeat-str [n strng]
  (str/join (repeat n strng)))

;https://www.codewars.com/kata/51f9d93b4095e0a7200001b8
(defn howManyLightsabersDoYouOwn
  [text]
  (if (str/includes? text "Zach") 18 0))

;https://www.codewars.com/kata/56a946cd7bd95ccab2000055
(defn lowercase_count[strng]
  (count (re-seq #"\p{Ll}" strng)))

;https://www.codewars.com/kata/525c1a07bb6dda6944000031
(def websites
  (take 1000 (repeat "codewars")))

;https://www.codewars.com/kata/568d0dd208ee69389d000016
(defn rental-car-cost [d]
  (cond
    (= d 0) (* d 40)
    (= d 1) (* d 40)
    (= d 2) (* d 40)
    (< d 7) (- (* d 40) 20)
    :else (- (* d 40) 50)))

;https://www.codewars.com/kata/56f6ad906b88de513f000d96
(defn bonus-time [salary bonus]
  (let [result (if bonus (* salary 10) salary)]
    (str "$" result)))

;https://www.codewars.com/kata/56fa3c5ce4d45d2a52001b3c
(defn xor [a,b]
  (not= a b))

;https://www.codewars.com/kata/55d24f55d7dd296eb9000030
(defn summation [n]
  (apply + (range (+ 1 n))))