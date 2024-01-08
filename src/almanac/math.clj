(ns almanac.math)

(defn dot [v1 v2]
  {:pre [(= (count v1) (count v2))]}
  (->> (map * v1 v2)
       (reduce + 0.0)))

(defn matrix-vector-product [m v]
  (vec (for [w m] (dot w v))))

(defn angle
  "Returns the angle of a vector in radians. The angle is measured
  anti-clockwise from the positive x-axis, with a branch cut on the negative
  y-axis."
  [x y]
  {:pre [(not (and (zero? x) (zero? y)))]}
  (cond
    (neg? x) (+ (angle (- x) (- y)) Math/PI)
    (neg? y) (- (angle x (- y)))
    (zero? x) (/ Math/PI 2)
    :else (Math/atan (/ y x))))

(defn solve-sine-cosine-equation
  "Returns the real values of θ in [0, 2π) satisfying the equation:
  a cos(θ) + b sin(θ) + c = 0."
  [a b c]
  (let [tolerance 1.0E-10
        discriminant (+ (Math/pow a 2)
                        (Math/pow b 2)
                        (- (Math/pow c 2)))]
    (cond
      ;; The discriminant is zero, except for floating point arithmetic.
      (>= tolerance (Math/abs discriminant))
        [(Math/atan (/ b a))]
      (pos? discriminant)
        (vec (for [sign [-1 1]]
               (let [real-part (+ (* -1 a c)
                                  (* sign b (Math/sqrt discriminant)))
                     imag-part (+ (* -1 b c)
                                  (* (- sign) a (Math/sqrt discriminant)))]
                 (angle real-part imag-part))))
      :else [])))

(defn newton
  "Finds a solution of f(x) = 0 numerically by Newton's method, with an initial
  guess of x."
  [f tolerance max-iterations x]
  (cond
    (>= tolerance (Math/abs (f x))) x
    (zero? max-iterations) (throw (Exception. "Failed to converge"))
    :else (let [f' (/ (- (f (+ x tolerance))
                         (f x))
                      tolerance)
                delta (- (/ (f x) f'))]
            (recur f tolerance (dec max-iterations) (+ x delta)))))

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
