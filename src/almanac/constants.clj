(ns almanac.constants
  (:require
    [clj-time.core :as t]))

(def TWO_PI (* 2 Math/PI))
(def SQRT_HALF (Math/sqrt 0.5))
(def HALF_PI (/ Math/PI 2))

(def WINTER_SOLSTICE_2023_UTC (t/date-time 2023 12 22 3 27))

(def ONE_YEAR_IN_DAYS 365.256) ;; From Wikipedia
(def ONE_DAY_IN_SECONDS 86400)
(def ONE_YEAR_IN_SECONDS (* ONE_YEAR_IN_DAYS ONE_DAY_IN_SECONDS))

(def SIDEREAL_DAY_IN_SECONDS 86164.0905) ;; Wikipedia
;; (/ ONE_YEAR_IN_SECONDS (inc ONE_YEAR_IN_DAYS))

;; Do we need to account for daylight savings time here?
;; Angle of longitude 0, in the sidereal frame.
(def AZIMUTHAL_ANGLE_WINTER_SOLSTICE_2023
  (-> (t/in-seconds (t/hours 3))
      (+ (t/in-seconds (t/minutes 27)))
      (/ SIDEREAL_DAY_IN_SECONDS)
      (* TWO_PI)))

(def EARTH_ROTATION_RADIANS_PER_SECOND
  (/ TWO_PI SIDEREAL_DAY_IN_SECONDS))

(def EARTH_AXIAL_TILT_RADIANS 0.409088)
(def EARTH_AXIAL_TILT_DEGREES 23.439)

(def SUN_HALF_WIDTH_OFFSET_DEGREES 0.83)
(def SUN_HALF_WIDTH_OFFSET_RADIANS 0.0145)

;;(def EARTH_ORBIT_PERIHELION_RADIANS 0.17)
(def EARTH_ORBIT_ECCENTRICITY 0.01671)

(def EARTH_PERIHELION_DATE_2024 (t/date-time 2024 1 3 0 38))
(def EARTH_ORBIT_PERIHELION_RADIANS (-> (t/interval WINTER_SOLSTICE_2023_UTC
                                                    EARTH_PERIHELION_DATE_2024)
                                        t/in-seconds
                                        (/ ONE_YEAR_IN_SECONDS)
                                        (* TWO_PI)))
