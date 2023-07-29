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


;https://www.codewars.com/kata/52f787eb172a8b4ae1000a34
(defn zeros
  ([n]
   (zeros n 0))
  ([n cnt]
   (if (< n 5)
     (int cnt)
     (recur (/ n 5) (+ cnt (/ n 5))))))

;https://www.codewars.com/kata/52685f7382004e774f0001f7
(defn formatDuration [secs]
  ; TODO
  (let [h (int (/ secs 3600))
        m (int (/ (- secs (* h 3600)) 60))
        s (- secs (* 3600 h) (* 60 m))]
    (format "%02d:%02d:%02d" h m s)))
