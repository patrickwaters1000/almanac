(ns almanac.solar-system
  "Models the motion of the Earth and Sun."
  (:require
    [almanac.angles :as a]
    [almanac.constants :as c]
    [almanac.ellipse :as ellipse]
    [almanac.math :refer [dot matrix-vector-product]]
    [almanac.time :refer [sidereal-days-since-winter-solstice-2023
                          years-since-winter-solstice-2023]]
    [clj-time.coerce :as tc]))

;; According to Wikipedia, Earth spins anticlockwise on its axis when viewed
;; from above the northern hemisphere.
(defn earth-rotational-angle [longitude dt]
  (+ c/AZIMUTHAL_ANGLE_WINTER_SOLSTICE_2023
     (a/degrees-to-radians longitude)
     (-> (sidereal-days-since-winter-solstice-2023 dt)
         (* c/TWO_PI))))

;; According to Wikipedia, Earth orbits the sun anti-clockwise when viewed from
;; above the Northern hemisphere.
(defn earth-orbital-angle
  "Calculates the angle in the xy-plane of the vector from the Sun to the Earth."
  [dt & {:keys [circular-orbit]}]
  (let [approximate-orbital-angle (-> (years-since-winter-solstice-2023 dt)
                                      (* c/TWO_PI))]
    (if circular-orbit
      approximate-orbital-angle
      (ellipse/adjust-orbital-angle
        approximate-orbital-angle
        c/EARTH_ORBIT_ECCENTRICITY
        c/EARTH_ORBIT_PERIHELION_RADIANS))))

(defn- earth-sun-vector
  "Calculates a unit vector in the direction from the Earth to the Sun."
  [dt & {:keys [circular-orbit]}]
  (let [angle (earth-orbital-angle dt :circular-orbit circular-orbit)]
    [(- (Math/cos angle))
     (- (Math/sin angle))
     0.0]))

(defn- spherical-local-frame
  "Expresses a local spherical coordinate local frame in cartesian coordinates.
  Returns a matrix whose rows represent unit vectors in the up, north and east
  directions."
  [θ ϕ]
  {:up [(* (Math/sin θ) (Math/cos ϕ))
        (* (Math/sin θ) (Math/sin ϕ))
        (Math/cos θ)]
   :north [(* -1.0 (Math/cos θ) (Math/cos ϕ))
           (* -1.0 (Math/cos θ) (Math/sin ϕ))
           (Math/sin θ)]
   :east [(* -1.0 (Math/sin ϕ))
          (Math/cos ϕ)
          0.0]})

;; On winter solstice, the northern hemisphere is tilting away from the Sun.
(defn- apply-axial-tilt [v]
  (let [α c/EARTH_AXIAL_TILT_RADIANS]
    (matrix-vector-product [[(Math/cos α) 0.0 (Math/sin α)]
                            [0.0 1.0 0.0]
                            [(- (Math/sin α)) 0.0 (Math/cos α)]]
                           v)))

(defn- map-vals [f m] (reduce-kv #(assoc %1 %2 (f %3)) {} m))

;; Not adjusted for atmospheric refraction.
(defn apparent-position-of-sun
  [lattitude longitude dt circular-orbit]
  (let [θ (a/polar-angle lattitude)
        ϕ (earth-rotational-angle longitude dt)
        {:keys [up north east]} (->> (spherical-local-frame θ ϕ)
                                     (map-vals apply-axial-tilt))
        v (earth-sun-vector dt :circular-orbit circular-orbit)]
    {:compass-angle (a/compass-angle (dot v east)
                                     (dot v north))
     :inclination-angle (a/inclination-angle (dot v east)
                                             (dot v north)
                                             (dot v up))}))
