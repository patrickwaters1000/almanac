(ns almanac.ellipse
  (:require
    [almanac.constants :as c]
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
  (if (>= 1.0E-9 (Math/abs (- theta c/HALF_PI)))
    (Math/sqrt (- (Math/pow a 2) 1))
    (let [c (Math/sqrt (- (Math/pow a 2) 1))
          tan-theta (Math/tan theta)
          tan-sq-theta (Math/pow tan-theta 2)
          A (+ (Math/pow a -2) tan-sq-theta)
          B (* 2 c tan-sq-theta)
          C (- (* (Math/pow c 2) tan-sq-theta) 1.0)
          xs (sort (math/solve-quadratic A B C))]
      (if (< theta c/HALF_PI)
        (last xs)
        (first xs)))))

(defn- ellipse-big-sector-area
  [a theta]
  {:pre [(<= 0 theta)
         (< theta c/HALF_PI)]}
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
         (< theta c/HALF_PI)]}
  (let [X (- (ellipse-focal-ray-intersection-x a (- Math/PI theta)))
        c (Math/sqrt (- (Math/pow a 2) 1))
        triangle-area (* 0.5
                         (Math/tan theta)
                         (Math/pow (- X c) 2))
        ellipse-quarter-area (* 0.25 a Math/PI)]
    (+ triangle-area
       (- ellipse-quarter-area
          (area-under-ellipse a X)))))

(defn- mod-out-full-orbits [angle]
  (let [remainder (-> (+ angle Math/PI)
                      (mod (* 2 Math/PI))
                      (- Math/PI))
        orbits (/ (- angle remainder)
                  (* 2 Math/PI))]
    {:orbits orbits
     :remainder remainder}))

(defn- line [[x1 y1] [x2 y2] x]
  (let [m (/ (- y2 y1)
             (- x2 x1))]
    (+ (* m (- x x1)) y1)))

(declare elliptic-sector-area)

(defn elliptic-sector-area-angle-near-pi-over-2 [a theta]
  {:pre [(>= 1.0E-8 (Math/abs (- theta c/HALF_PI)))]}
  (let [theta-1 (- c/HALF_PI 2.0E-8)
        theta-2 (+ c/HALF_PI 2.0E-8)
        area-1 (elliptic-sector-area a theta-1)
        area-2 (elliptic-sector-area a theta-2)]
    (line [theta-1 area-1]
          [theta-2 area-2]
          theta)))

(defn elliptic-sector-area
  "Calculates the area of a sector of an ellipse. The ellipse is defined by

  (x/a)^2 + y^2 = 1, with a > 1.

  Such an ellipse has a focus at F = (c, 0), where c = sqrt(a^2 - 1).  The
  sector is defined by an angle about F. The angle is measured anticlockwise
  from the positive x-axis."
  [a theta]
  {:pre [(>= a 1)]}
  (cond
    (<= Math/PI theta) (let [{:keys [orbits remainder]}
                               (mod-out-full-orbits theta)
                             ellipse-area (* a Math/PI)]
                         (+ (elliptic-sector-area a remainder)
                            (* orbits ellipse-area)))
    (neg? theta) (- (elliptic-sector-area a (- theta)))
    (>= 1.0E-8
        (Math/abs
          (- theta
             c/HALF_PI))) (elliptic-sector-area-angle-near-pi-over-2 a theta)
    (< theta c/HALF_PI) (ellipse-small-sector-area a theta)
    ;; PI/2 < theta < PI
    :else (let [ellipse-half-area (* 0.5 a Math/PI)]
            (- ellipse-half-area
               (ellipse-big-sector-area
                 a (- Math/PI theta))))))

(defn orbital-angle
  "Adjusts an orbital angle computed with a circular orbit approximation to
  account for an elliptical orbit with eccentricity `e` and perihelion alpha
  (i.e., nearest point to the sun at angle alpha)."
  [e alpha t]
  (let [a (Math/pow (- 1.0 (Math/pow e 2))
                    -0.5)
        total-area (* a Math/PI)
        target-angle (- t alpha)
        target-area (* (/ target-angle (* 2 Math/PI))
                       total-area)
        f #(- (elliptic-sector-area a %)
              target-area)
        t-min (- target-angle c/HALF_PI)
        t-max (+ target-angle c/HALF_PI)]
    (+ alpha
       (math/solve-increasing-function
         f 1.0E-8 t-min t-max))))
