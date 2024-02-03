(ns almanac.math)

(defn dot [v1 v2]
  {:pre [(= (count v1) (count v2))]}
  (->> (map * v1 v2)
       (reduce + 0.0)))

(defn matrix-vector-product [m v]
  (vec (for [w m] (dot w v))))

(defn newton
  "Finds a solution of f(x) = 0 numerically by Newton's method, with an initial
  guess of x."
  [f dx tolerance max-iterations x]
  (cond
    (>= tolerance (Math/abs (f x))) x
    (zero? max-iterations) (throw (Exception. "Failed to converge"))
    :else (let [f' (/ (- (f (+ x dx))
                         (f x))
                      dx)
                delta (- (/ (f x) f'))]
            (recur f dx tolerance (dec max-iterations) (+ x delta)))))

(defn solve-quadratic
  [a b c]
  (let [discriminant (- (Math/pow b 2) (* 4 a c))
        center (/ (- b) (* 2 a))
        radius (when (pos? discriminant)
                 (/ (Math/sqrt discriminant)
                    (* 2 a)))]
    (cond
      (neg? discriminant) []
      (zero? discriminant) [center]
      :else [(- center radius) (+ center radius)])))

(defn solve-increasing-function
  "Finds the solution to f(x) = 0, where x is an increasing continuous function."
  [f tolerance a b]
  (let [c (/ (+ a b) 2.0)
        y-a (f a)
        y-b (f b)
        y-c (f c)]
    (cond
      (or (pos? y-a)
          (neg? y-b)) (throw (Exception.
                               (format "There is no solution in [%f, %f]" a b)))
      (>= tolerance (Math/abs y-c)) c
      :else (let [[a b] (if (pos? y-c)
                          [a c]
                          [c b])]
              (recur f tolerance a b)))))

;;(defn fractions
;;  ([]
;;   (fractions #{} 1 2))
;;  ([history i d]
;;   (let [f (/ i d)
;;         [i d] (if (< (inc i) d)
;;                 [(inc i) d]
;;                 [1 (inc d)])]
;;     (if (history f)
;;       (fractions history i d)
;;       (lazy-seq (cons f (fractions (conj history f) i d)))))))
;;
;;(defn find-two-solutions
;;  [f tolerance a b max-attempts]
;;  (loop [xs (fractions)
;;         solutions []]
;;    (let [[x & xs] xs
;;          c (+ a (* x (- b a)))
;;          solution (newton f tolerance 20 c)])))
