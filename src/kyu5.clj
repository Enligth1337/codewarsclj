(ns kyu5)

;https://www.codewars.com/kata/559a28007caad2ac4e000083
(defn fibb [x]
  (reduce (fn [acc x]
            (if (or (= x 1) (= x 2))
              (conj acc 1)
              (conj acc (+ (bigint (first acc)) (bigint (second acc))))))
          '() x))

(defn perimeter [n]
  (let [sq-cnt (rest (range (+ n 2)))]
    (->> sq-cnt
         fibb
         (apply +)
         (* 4))))



