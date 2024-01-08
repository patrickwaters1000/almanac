(ns almanac.core
  (:require
    [almanac.constants :as c]
    [almanac.ellipse :as ellipse]
    [almanac.math :as math]
    [almanac.sunrise-equation :as sunrise-equation]
    [almanac.units :as u]
    [clj-time.coerce :as tc]
    [clj-time.core :as t]
    [clj-time.format :as tf]))

;; TODO
;; Precession of Earth's orbit
;; Interaction of Earth and moon
;; Precession of Earth's axial tilt?

(defrecord Config
  [lattitude
   longitude
   time-zone
   date
   circular-orbit
   coarse-sunrise-equation])

;; According to Wikipedia, Earth orbits the sun anti-clockwise when viewed from
;; above the Northern hemisphere.
(defn- earth-sun-angle
  "Calculates the angle in the xy-plane of the vector from the Earth to the Sun."
  [dt & {:keys [circular-orbit]}]
  (let [t (u/date-to-years dt)
        _ (println (format "Date: %s" dt))
        _ (println (format "Date in years: %f" t))
        base-sun-earth-angle (* 2 Math/PI t)
        sun-earth-angle (if circular-orbit
                          base-sun-earth-angle
                          (ellipse/orbital-angle
                            c/EARTH_ORBIT_ECCENTRICITY
                            c/EARTH_ORBIT_PERIHELION_RADIANS
                            base-sun-earth-angle))]
    (println "Base sun earth angle: " base-sun-earth-angle)
    (println "Corrected sun earth angle: " sun-earth-angle)
    (+ Math/PI sun-earth-angle)))

(defn- earth-sun-vector
  "Calculates a unit vector in the direction from the Earth to the Sun."
  [dt & {:keys [circular-orbit]}]
  (let [γ (earth-sun-angle dt)
        γ (if circular-orbit
             γ
             (ellipse/orbital-angle 0.016 -0.17))]
    [(Math/cos γ)
     (Math/sin γ)
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

(defn- apparent-position-of-sun
  [lattitude longitude dt & {:keys [circular-orbit]}]
  (let [θ (compute-θ lattitude)
        ϕ (compute-ϕ longitude dt)
        {:keys [up
                north
                east]} (map (partial apply-axial-tilt
                                     c/EARTH_AXIAL_TILT_RADIANS)
                            (spherical-local-frame θ ϕ))
        v (earth-sun-vector dt :circular-orbit circular-orbit)]
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

(defn- compute-sunrise-and-sunset
  [^Config conf]
  (let [{:keys [lattitude
                longitude
                date
                time-zone
                circular-orbit
                coarse-sunrise-eq]} conf
        θ (compute-θ lattitude)
        γ0 (earth-sun-angle date :circular-orbit circular-orbit)]
    (println (format "Corrected earth sun angle: %f" γ0))
    (->> (sunrise-equation/solve-for-ϕ θ γ0 :coarse coarse-sunrise-eq)
         (map (partial compute-time-of-day-seconds longitude time-zone date))
         (sort)
         (map format-time))))

(declare cli-spec
         parse-args)

(defn -main [& args]
  (let [opts (parse-args cli-spec args)
        [sunrise sunset] (compute-sunrise-and-sunset opts)]
    (println (format "Sunrise: %s\nSunset: %s" sunrise sunset))))

(defn parse-args [cli-spec args]
  (->> args
       (partition 2)
       (map (fn [[k v]]
              (let [{:keys [arg parse-fn]} (->> cli-spec
                                                (filter #(= k (:arg %)))
                                                first)
                    [_ k*] (re-matches #"\-\-(.+)" arg)
                    v* (if parse-fn
                         (parse-fn v)
                         v)]
                [(keyword k*) v*])))
       (into {})
       map->Config))

(def cli-spec
  [{:arg "--lattitude"
    :description "REQUIRED. Lattitude in degrees."
    :parse-fn #(Double/parseDouble %)}
   {:arg "--longitude"
    :description "REQUIRED. Longitude in degrees."
    :parse-fn #(Double/parseDouble %)}
   {:arg "--time-zone"
    :description "REQUIRED. Time zone, as a number of hours after UTC."
    :parse-fn #(Integer/parseInt %)}
   {:arg "--date"
    :description "REQUIRED. Date"
    :parse-fn #(tf/parse (:date tf/formatters) %)}
   {:arg "--coarse-sunrise-eq"
    :description (str "OPTIONAL. When true, don't account for the motion of "
                      "Earth around the Sun during the day.")
    :parse-fn #(Boolean/parseBoolean %)}
   {:arg "--coarse-sunrise-eq"
    :description (str "OPTIONAL. When true, model Earth's orbit as a circle "
                      "instead of an ellipse.")
    :parse-fn #(Boolean/parseBoolean %)}])
