(ns almanac.angles)

(defn radians-to-degrees [r]
  (* r (/ 180.0 Math/PI)))

(defn degrees-to-radians [d]
  (* d (/ Math/PI 180.0)))

(defn polar-angle [lattitude]
  (- (/ Math/PI 2.0) (degrees-to-radians lattitude)))

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

(defn compass-angle
  "Returns an angle measured clockwise from North, in degrees."
  [east-component north-component]
  (mod (- 90.0
          (radians-to-degrees
            (angle east-component
                   north-component)))
       360.0))

(defn inclination-angle
  "Returns an angle of inclination between -90 degrees and 90 degrees. Negative
  values represent inclinations below the horizon."
  [east-component north-component up-component]
  (let [forward-component (Math/sqrt (+ (Math/pow east-component 2)
                                        (Math/pow north-component 2)))]
    (radians-to-degrees (angle forward-component up-component))))
