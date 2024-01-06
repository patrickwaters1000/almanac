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
