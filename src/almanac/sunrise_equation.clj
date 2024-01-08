(ns almanac.sunrise-equation
  "Solves for the angle of Earth about its axis at sunrise and sunset. This
  angle is measured anticlockwise from the positive x-direction, in coordinates
  where:

  * the Sun is at the origin,
  * the Earth revolves anticlockwise around the Sun in the xy-plane,
  * the Earth is on the positive x-axis at the beginning of January 1.

  The equation is:

  0 = cos(α) sin(θ) cos(γ(ϕ)) cos(ϕ)
      + sin(θ) sin(γ(ϕ)) sin(ϕ)
      + sin(α) cos(θ) cos(γ(ϕ))
      + SUN_HALF_WIDTH_OFFSET,

  where

  α = EARTH_AXIAL_TILT_RADIANS,
  θ = angle from true north pole to lattitude,
  γ = angle of Earth in orbit around Sun
    = γ0 + ϕ / ONE_YEAR_IN_DAYS,
  ϕ = angle of Earth around axis."
  (:require
    [almanac.constants :as c]
    [almanac.math :as math]))

(defn- a [θ γ]
  (let [α c/EARTH_AXIAL_TILT_RADIANS]
    (* (Math/cos α) (Math/sin θ) (Math/cos γ))))

(defn- b [θ γ]
  (* (Math/sin γ) (Math/sin θ)))

(defn- c [θ γ]
  (let [α c/EARTH_AXIAL_TILT_RADIANS]
    (+ (* (Math/sin α) (Math/cos θ) (Math/cos γ))
       (* c/SUN_HALF_WIDTH_OFFSET_RADIANS))))

(defn- solve-for-ϕ-coarse [θ γ]
  (math/solve-sine-cosine-equation (a θ γ)
                                   (b θ γ)
                                   (c θ γ)))

(defn- error [θ γ0 ϕ]
  (let [γ (+ γ0 (/ ϕ c/ONE_YEAR_IN_DAYS))]
    (+ (* (a θ γ) (Math/cos ϕ))
       (* (b θ γ) (Math/sin ϕ))
       (c θ γ))))

(defn solve-for-ϕ [θ γ0 & {:keys [coarse]}]
  (let [[ϕ0-sunrise
         ϕ0-sunset] (solve-for-ϕ-coarse θ γ0)
        f (partial error θ γ0)]
    (if coarse
      [ϕ0-sunrise
       ϕ0-sunset]
      (for [ϕ0 [ϕ0-sunrise ϕ0-sunset]]
        (math/newton f 1.0E-8 20 ϕ0)))))
