(ns almanac.ellipse
  (:require
    [almanac.math :as math]))

(defn- area-under-ellipse
  "Calculate the area under the ellipse (X/a)^2 + Y^2 = 1, and above the x-axis,
  from X = 0 to X = x."
  [a x]
  (+ (* 0.5 x (Math/sqrt (- 1.0 (Math/pow (/ x a) 2))))
     (* 0.5 a (Math/asin (/ x a)))))

(defn- ellipse-focal-ray-intersection-x
  "Computes the x-coordinate of the intersection of a ray with an ellipse. The
  ellipse is defined by (x/a)^2 + y^2 = 1 with a > 1. Such an ellipse has a
  focus at F = (-c, 0), where c = sqrt(a^2 - 1).  The ray originates from F at
  angle theta, measured anticlockwise from the positive x-direction."
  [a theta]
  {:pre [(not (neg? theta))
         (<= theta Math/PI)]}
  (if (>= 1.0E-9 (Math/abs (- theta (/ Math/PI 2))))
    (Math/sqrt (- (Math/pow a 2) 1))
    (let [c (Math/sqrt (- (Math/pow a 2) 1))
          tan-theta (Math/tan theta)
          tan-sq-theta (Math/pow tan-theta 2)
          A (+ (Math/pow a -2) tan-sq-theta)
          B (* 2 c tan-sq-theta)
          C (- (* (Math/pow c 2) tan-sq-theta) 1.0)
          xs (sort (math/solve-quadratic A B C))]
      (when (empty? xs)
        (println (format "No solutions to ray intersection eq with a=%s, theta=%s" a theta)))
      (if (< theta (/ Math/PI 2))
        (last xs)
        (first xs)))))

(defn- ellipse-big-sector-area
  [a theta]
  {:pre [(<= 0 theta)
         (< theta (/ Math/PI 2))]}
  (let [X (ellipse-focal-ray-intersection-x a theta)
        c (Math/sqrt (- (Math/pow a 2) 1))
        triangle-area (* 0.5
                         (Math/tan theta)
                         (Math/pow (+ X c) 2))
        ellipse-quarter-area (* 0.25 a Math/PI)]
    (+ triangle-area
       (- ellipse-quarter-area
          (area-under-ellipse a X)))))

(defn- ellipse-small-sector-area
  [a theta]
  {:pre [(<= 0 theta)
         (< theta (/ Math/PI 2))]}
  (let [X (- (ellipse-focal-ray-intersection-x a (- Math/PI theta)))
        c (Math/sqrt (- (Math/pow a 2) 1))
        triangle-area (* 0.5
                         (Math/tan theta)
                         (Math/pow (- X c) 2))
        ellipse-quarter-area (* 0.25 a Math/PI)]
    (+ triangle-area
       (- ellipse-quarter-area
          (area-under-ellipse a X)))))

;;(catch Exception _ (throw (Exception. (format "Failed with a=%s, theta=%s" a theta))))

(defn elliptic-sector-area
  "Calculates the area of a sector of an ellipse. The ellipse is defined by

  (x/a)^2 + y^2 = 1, with a > 1.

  Such an ellipse has a focus at F = (-c, 0), where c = sqrt(a^2 - 1).  The
  sector is defined by an angle about F. The angle is measured anticlockwise
  from the positive x-axis."
  [a theta]
  {:pre [(>= a 1)]}
  (cond
    (or (neg? theta)
        (<= (* 2 Math/PI) theta)) (let [remainder (mod theta (* 2 Math/PI))
                                        orbits (/ (- theta remainder)
                                                  (* 2 Math/PI))
                                        ellipse-area (* a Math/PI)]
                                    (+ (elliptic-sector-area a remainder)
                                       (* orbits ellipse-area)))
    (<= Math/PI theta) (let [ellipse-half-area (* 0.5 a Math/PI)]
                         (+ ellipse-half-area
                            (elliptic-sector-area
                              a (- theta Math/PI))))
    (< theta (/ Math/PI 2)) (ellipse-small-sector-area a theta)
    (= theta (/ Math/PI 2)) (let [c (Math/sqrt (- (Math/pow a 2) 1))]
                              (- (area-under-ellipse a a)
                                 (area-under-ellipse a c)))
    ;; PI/2 < theta < PI
    :else (let [ellipse-half-area (* 0.5 a Math/PI)]
            (- ellipse-half-area
               (ellipse-big-sector-area
                 a (- Math/PI theta))))))

(defn orbital-angle
  "Adjusts an orbital angle computed with a circular orbit approximation to
  account for an elliptical orbit with eccentricity `e` and aphelion alpha
  (i.e., furthest point from sun at angle alpha)."
  [e alpha t]
  (let [a (Math/pow (- 1.0 (Math/pow e 2))
                    -0.5)
        total-area (* a Math/PI)
        target-angle (- t alpha)
        target-area (* (/ target-angle (* 2 Math/PI))
                       total-area)
        f #(- (elliptic-sector-area a %)
              target-area)
        t-min (- target-angle (/ Math/PI 2))
        t-max (+ target-angle (/ Math/PI 2))]
    (+ alpha
       (math/solve-increasing-function
         f 1.0E-8 t-min t-max))))
