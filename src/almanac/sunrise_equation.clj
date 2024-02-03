(ns almanac.sunrise-equation
  "Solves for the angle of Earth about its axis at sunrise and sunset. This
  angle is measured anticlockwise from the positive x-direction, in coordinates
  where:

  * the Sun is at the origin,
  * the Earth revolves anticlockwise around the Sun in the xy-plane,
  * the Earth is on the positive x-axis at the winter solstice.

  The equation is:

  0 = cos(α) sin(θ) cos(γ) cos(ϕ)
      + sin(θ) sin(γ) sin(ϕ)
      + sin(α) cos(θ) cos(γ)
      + SUN_HALF_WIDTH_OFFSET,

  where

  α = EARTH_AXIAL_TILT_RADIANS,
  θ = angle from true north pole to lattitude,
  γ = angle of Earth in orbit around Sun
  ϕ = angle of Earth around axis.

  In this namespace we ignore that there is a relation between γ and
  ϕ. Basically we treat γ as a constant. This means we are ignoring the motion
  of Earth around the sun during the day."
  (:require
    [almanac.angles :refer [angle polar-angle]]
    [almanac.constants :as c]
    [almanac.solar-system :refer [earth-rotational-angle
                                  earth-orbital-angle]]
    [clj-time.coerce :as tc]
    [clj-time.core :as t]))

(defn- a [θ γ]
  (let [α c/EARTH_AXIAL_TILT_RADIANS]
    (* (Math/cos α) (Math/sin θ) (Math/cos γ))))

(defn- b [θ γ]
  (* (Math/sin γ) (Math/sin θ)))

(defn- c [θ γ]
  (let [α c/EARTH_AXIAL_TILT_RADIANS]
    (- (* (Math/sin α) (Math/cos θ) (Math/cos γ))
       (* c/SUN_HALF_WIDTH_OFFSET_RADIANS))))

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

(defn solve-for-earth-rotational-angle [θ γ]
  (solve-sine-cosine-equation (a θ γ)
                              (b θ γ)
                              (c θ γ)))

;;(defn- approx-noon [longitude date]
;;  (let [approx-timezone (quot longitude 15)]
;;    (-> date
;;        (t/plus (t/hours approx-time-zone))
;;        (t/plus (t/hours 12) ))))

(defn- angle-branch-fn [min-angle]
  (fn [angle]
    (-> angle
        (- min-angle)
        (mod c/TWO_PI)
        (+ min-angle))))

(defn- compute-datetime-for-earth-rotational-angle
  "Computes the datetime at which a rotational angle of ϕ will be achieved, on
  the specified date."
  [longitude date ϕ]
  (let [ϕ-midnight (earth-rotational-angle longitude date)
        seconds-since-midnight (/ (mod (- ϕ ϕ-midnight) c/TWO_PI)
                                  c/EARTH_ROTATION_RADIANS_PER_SECOND)]
    (t/plus date (t/seconds seconds-since-midnight))))

(defn calculate-approximate-sunrise-and-sunset
  "Calculates approximate sunrise and sunset times in GMT time, not accounting
  for the motion of the Earth around the Sun during the input date."
  [lattitude longitude date circular-orbit]
  (let [θ (polar-angle lattitude)
        γ (earth-orbital-angle date :circular-orbit circular-orbit)]
    (->> (solve-for-earth-rotational-angle θ γ)
         (sort-by (angle-branch-fn γ))
         (map (partial compute-datetime-for-earth-rotational-angle
                       longitude
                       date)))))
