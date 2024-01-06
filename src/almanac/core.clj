(ns almanac.core
  (:require
    [almanac.constants :as c]
    [almanac.math :as math]
    [almanac.sunrise-equation :as sunrise-equation]
    [almanac.units :as u]
    [clj-time.coerce :as tc]
    [clj-time.core :as t]))

;; TODO
;; Eccentricity of Earth's orbit
;; Precession of Earth's orbit
;; Interaction of Earth and moon
;; Precession of Earth's axial tilt

;; According to Wikipedia, Earth orbits the sun anti-clockwise when viewed from
;; above the Northern hemisphere.
(defn- earth-sun-angle
  "Calculates the angle in the xy-plane of the vector from the Earth to the Sun."
  [dt]
  (let [t (u/date-to-years dt)]
    (+ Math/PI (* 2 Math/PI t))))

(defn- earth-sun-vector
  "Calculates a unit vector in the direction from the Earth to the Sun."
  [dt]
  (let [γ (earth-sun-angle dt)]
    [(Math/cos γ)
     (Math/sin γ)
     0.0]))

(defn- spherical-local-frame
  "Expresses a local spherical coordinate local frame in cartesian coordinates.
  Returns a matrix whose rows represent unit vectors in the up, north and east
  directions."
  [θ ϕ]
  (let [up [(* (Math/sin θ) (Math/cos ϕ))
            (* (Math/sin θ) (Math/sin ϕ))
            (Math/cos θ)]
        north [(* -1.0 (Math/cos θ) (Math/cos ϕ))
               (* -1.0 (Math/cos θ) (Math/sin ϕ))
               (Math/sin θ)]
        east [(* -1.0 (Math/sin ϕ))
              (Math/cos ϕ)
              0.0]]
    [up north east]))

;; On January 1, the northern hemisphere is tilting away from the Sun. So the
;; Earth's axial tilt is a rotation of 23 degrees CLOCKWISE in the xz-plane.
(defn- apply-axial-tilt [α v]
  (math/matrix-vector-product [[(Math/cos α) 0.0 (Math/sin α)]
                               [0.0 1.0 0.0]
                               [(- (Math/sin α)) 0.0 (Math/cos α)]]
                              v))

(defn- compute-θ [lattitude]
  (- (/ Math/PI 2.0) (u/degrees-to-radians lattitude)))

(defn- compute-ϕ [longitude dt]
  (- (u/degrees-to-radians longitude)
     (* 2 Math/PI (u/date-to-days dt))))

(defn- compass-angle
  "Returns an angle measured clockwise from North, in degrees."
  [east-component north-component]
  (mod (- 90.0
          (u/radians-to-degrees
            (math/angle east-component
                        north-component)))
       360.0))

(defn- inclination-angle
  "Returns an angle of inclination between -90 degrees and 90 degrees. Negative
  values represent inclinations below the horizon."
  [east-component north-component up-component]
  (let [forward-component (Math/sqrt (+ (Math/pow east-component 2)
                                        (Math/pow north-component 2)))]
    (u/radians-to-degrees (math/angle forward-component up-component))))

(defn- apparent-position-of-sun [lattitude longitude dt]
  (let [θ (compute-θ lattitude)
        ϕ (compute-ϕ longitude dt)
        [up north east] (map (partial apply-axial-tilt c/EARTH_AXIAL_TILT_RADIANS)
                             (spherical-local-frame θ ϕ))
        v (earth-sun-vector dt)]
    {:compass-angle (compass-angle (math/dot v east)
                                   (math/dot v north))
     :inclination-angle (inclination-angle (math/dot v east)
                                           (math/dot v north)
                                           (math/dot v up))}))

(defn- format-time
  "Expresses a number of seconds as a time of day string."
  [total-seconds]
  (let [hours (int (quot total-seconds 3600))
        total-seconds (mod total-seconds 3600)
        minutes (int (quot total-seconds 60))
        seconds (int (mod total-seconds 60))]
    (format "%d:%02d:%02d" hours minutes seconds)))

(defn- compute-time-of-day-seconds
  [longitude time-zone-hours dt ϕ]
  (-> ϕ
      (- (u/degrees-to-radians longitude))
      (/ (* 2 Math/PI))
      (- (u/date-to-years dt))
      (+ (/ (float time-zone-hours) 24))
      (mod 1.0)
      (* c/ONE_DAY_IN_SECONDS)))

(defn- compute-sunrise-and-sunset [lattitude longitude time-zone-hours dt & {:keys [coarse]}]
  (let [θ (compute-θ lattitude)
        γ0 (earth-sun-angle dt)]
    (->> (if coarse
           (sunrise-equation/solve-for-ϕ-coarse θ γ0)
           (sunrise-equation/solve-for-ϕ θ γ0))
         (map (partial compute-time-of-day-seconds longitude time-zone-hours dt))
         (sort)
         (map format-time))))
