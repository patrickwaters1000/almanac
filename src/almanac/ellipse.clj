(ns almanac.ellipse
  "Exposes a function for correcting the orbital angle of a celestial body to
  account for the eccentricity of its orbit."
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
  (let [c (Math/sqrt (- (Math/pow a 2) 1))
        sin-theta (Math/sin theta)
        cos-theta (Math/cos theta)
        sin-sq-theta (Math/pow sin-theta 2)
        cos-sq-theta (Math/pow cos-theta 2)
        A (+ (* (Math/pow a -2) cos-sq-theta)
             sin-sq-theta)
        B (* 2 c sin-sq-theta)
        C (- (* (Math/pow c 2) sin-sq-theta)
             cos-sq-theta)
        xs (sort (math/solve-quadratic A B C))]
    (if (< theta c/HALF_PI)
      (last xs)
      (first xs))))

(defn- ellipse-big-sector-area
  [a theta]
  {:pre [(<= 0 theta)
         (<= theta c/HALF_PI)]}
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

(defn- elliptic-sector-area
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
    (< theta c/HALF_PI) (ellipse-small-sector-area a theta)
    ;; PI/2 < theta < PI
    :else (let [ellipse-half-area (* 0.5 a Math/PI)]
            (- ellipse-half-area
               (ellipse-big-sector-area
                 a (- Math/PI theta))))))

(defn adjust-orbital-angle
  "Adjusts an orbital angle computed with a circular orbit approximation to
  account for an elliptical orbit with eccentricity = e and perihelion = alpha
  (i.e., nearest point to the sun at angle alpha)."
  [coarse-angle e alpha]
  (let [a (Math/pow (- 1.0 (Math/pow e 2))
                    -0.5)
        total-area (* a Math/PI)
        target-angle (- coarse-angle alpha)
        target-area (* (/ target-angle (* 2 Math/PI))
                       total-area)
        f #(- (elliptic-sector-area a %)
              target-area)
        angle-min (- target-angle c/HALF_PI)
        angle-max (+ target-angle c/HALF_PI)]
    (+ alpha
       (math/solve-increasing-function
         f 1.0E-8 angle-min angle-max))))
