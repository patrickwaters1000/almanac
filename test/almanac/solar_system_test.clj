(ns almanac.solar-system-test
  (:require
    [almanac.constants :as c]
    [almanac.solar-system :as s]
    [almanac.testlib :refer [approx?
                             approximately-equal-vectors?
                             approximately-equal-maps?
                             approximately-equal-matrices?]]
    [clj-time.core :as t]
    [clojure.test :refer [is deftest]]))

(deftest computing-seconds-since-winter-solstice-2023
  (is (= (float (+ (* (- 32 23) 86400)
                   (* (- 24 4) 3600)
                   (* (- 60 27) 60)))
         (#'s/seconds-since-winter-solstice-2023 (t/date-time 2024)))))

(deftest computing-sidereal-days-since-winter-solstice-2023
  (is (approx? 1.0E-6
               (#'s/sidereal-days-since-winter-solstice-2023
                (t/plus c/WINTER_SOLSTICE_2023_UTC
                        (t/years 1)))
               (/ (* 366 c/ONE_DAY_IN_SECONDS)
                  c/SIDEREAL_DAY_IN_SECONDS))))

(deftest computing-earth-rotational-angle
  (is (approx? 0.02
               (mod (s/earth-rotational-angle 0 (t/date-time 2024 3 22))
                    c/TWO_PI)
               c/HALF_PI))
  (is (approx? 0.02
               (mod (s/earth-rotational-angle 90 (t/date-time 2024 3 22))
                    c/TWO_PI)
               Math/PI)))

(deftest computing-earth-orbital-angle
  (is (approx? 0.02 c/HALF_PI (s/earth-orbital-angle (t/date-time 2024 3 22)
                                                     :circular-orbit true))))

(deftest computing-earth-sun-vector
  (is (approximately-equal-vectors?
        1.0E-6
        [-1.0 0.0 0.0]
        (#'s/earth-sun-vector (t/date-time 2024 1))))
  (is (approximately-equal-vectors?
        0.02
        [0.0 -1.0 0.0]
        (#'s/earth-sun-vector (t/date-time 2024 4))))
  (is (approximately-equal-vectors?
        0.02
        [1.0 0.0 0.0]
        (#'s/earth-sun-vector (t/date-time 2024 7))))
  (is (approximately-equal-vectors?
        0.02
        [0.0 1.0 0.0]
        (#'s/earth-sun-vector (t/date-time 2024 10)))))

(deftest spherical-local-frame
  (is (approximately-equal-matrices?
        1.0E-6
        [[1.0 0.0 0.0]
         [0.0 0.0 1.0]
         [0.0 1.0 0.0]]
        (#'s/spherical-local-frame (/ Math/PI 2.0) 0.0)))
  (is (approximately-equal-matrices?
        1.0E-6
        [[0.0 1.0 0.0]
         [0.0 0.0 1.0]
         [-1.0 0.0 0.0]]
        (#'s/spherical-local-frame (/ Math/PI 2.0) (/ Math/PI 2.0))))
  (is (approximately-equal-matrices?
        1.0E-6
        [[c/SQRT_HALF 0.0 c/SQRT_HALF]
         [(- c/SQRT_HALF) 0.0 c/SQRT_HALF]
         [0.0 1.0 0.0]]
        (#'s/spherical-local-frame (/ Math/PI 4.0) 0.0)))
  (is (approximately-equal-matrices?
        1.0E-6
        [[0.0 c/SQRT_HALF c/SQRT_HALF]
         [0.0 (- c/SQRT_HALF) c/SQRT_HALF]
         [-1.0 0.0 0.0]]
        (#'s/spherical-local-frame (/ Math/PI 4.0) (/ Math/PI 2.0)))))

(deftest applying-axial-tilt
  (is (approximately-equal-vectors?
        1.0E-6
        [(Math/cos c/EARTH_AXIAL_TILT_RADIANS)
         0.0
         (- (Math/sin c/EARTH_AXIAL_TILT_RADIANS))]
        (#'s/apply-axial-tilt c/EARTH_AXIAL_TILT_RADIANS
         [1.0 0.0 0.0])))
  (is (approximately-equal-vectors?
        1.0E-6
        [(Math/sin c/EARTH_AXIAL_TILT_RADIANS)
         0.0
         (Math/cos c/EARTH_AXIAL_TILT_RADIANS)]
        (#'s/apply-axial-tilt c/EARTH_AXIAL_TILT_RADIANS
         [0.0 0.0 1.0])))
  (is (approximately-equal-vectors?
        1.0E-6
        [0.0 1.0 0.0]
        (#'s/apply-axial-tilt c/EARTH_AXIAL_TILT_RADIANS
         [0.0 1.0 0.0]))))

(deftest computing-the-apparent-position-of-the-sun
  ;; Midnight GMT, at equator on GMT longitude line. Sun would be straight down,
  ;; except for axial tilt.
  (is (= {:compass-angle 180.0
          :inclination-angle (- c/EARTH_AXIAL_TILT_DEGREES 90.0)}
         (s/apparent-position-of-sun 0.0 0.0 (t/date-time 2024))))
  (is (approximately-equal-maps?
        1.0E-6
        {:compass-angle (- 270.0 c/EARTH_AXIAL_TILT_DEGREES)
         :inclination-angle 0.0}
        (s/apparent-position-of-sun 0.0 -90.0 (t/date-time 2024)))))
