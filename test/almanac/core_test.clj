(ns almanac.core-test
  (:require
    [almanac.constants :as c]
    [almanac.core :as core]
    [almanac.testlib :refer [approximately-equal-maps?
                             approximately-equal-vectors?
                             approximately-equal-matrices?]]
    [clj-time.core :as t]
    [clojure.test :refer [is deftest]]))

(deftest computing-earth-sun-vector
  (is (approximately-equal-vectors?
        1.0E-6
        [-1.0 0.0 0.0]
        (#'core/earth-sun-vector (t/date-time 2024 1))))
  (is (approximately-equal-vectors?
        0.02
        [0.0 -1.0 0.0]
        (#'core/earth-sun-vector (t/date-time 2024 4))))
  (is (approximately-equal-vectors?
        0.02
        [1.0 0.0 0.0]
        (#'core/earth-sun-vector (t/date-time 2024 7))))
  (is (approximately-equal-vectors?
        0.02
        [0.0 1.0 0.0]
        (#'core/earth-sun-vector (t/date-time 2024 10)))))

(deftest spherical-local-frame
  (is (approximately-equal-matrices?
        1.0E-6
        [[1.0 0.0 0.0]
         [0.0 0.0 1.0]
         [0.0 1.0 0.0]]
        (#'core/spherical-local-frame (/ Math/PI 2.0) 0.0)))
  (is (approximately-equal-matrices?
        1.0E-6
        [[0.0 1.0 0.0]
         [0.0 0.0 1.0]
         [-1.0 0.0 0.0]]
        (#'core/spherical-local-frame (/ Math/PI 2.0) (/ Math/PI 2.0))))
  (is (approximately-equal-matrices?
        1.0E-6
        [[c/SQRT_HALF 0.0 c/SQRT_HALF]
         [(- c/SQRT_HALF) 0.0 c/SQRT_HALF]
         [0.0 1.0 0.0]]
        (#'core/spherical-local-frame (/ Math/PI 4.0) 0.0)))
  (is (approximately-equal-matrices?
        1.0E-6
        [[0.0 c/SQRT_HALF c/SQRT_HALF]
         [0.0 (- c/SQRT_HALF) c/SQRT_HALF]
         [-1.0 0.0 0.0]]
        (#'core/spherical-local-frame (/ Math/PI 4.0) (/ Math/PI 2.0)))))

(deftest applying-axial-tilt
  (is (approximately-equal-vectors?
        1.0E-6
        [(Math/cos c/EARTH_AXIAL_TILT_RADIANS)
         0.0
         (- (Math/sin c/EARTH_AXIAL_TILT_RADIANS))]
        (#'core/apply-axial-tilt c/EARTH_AXIAL_TILT_RADIANS
         [1.0 0.0 0.0])))
  (is (approximately-equal-vectors?
        1.0E-6
        [(Math/sin c/EARTH_AXIAL_TILT_RADIANS)
         0.0
         (Math/cos c/EARTH_AXIAL_TILT_RADIANS)]
        (#'core/apply-axial-tilt c/EARTH_AXIAL_TILT_RADIANS
         [0.0 0.0 1.0])))
  (is (approximately-equal-vectors?
        1.0E-6
        [0.0 1.0 0.0]
        (#'core/apply-axial-tilt c/EARTH_AXIAL_TILT_RADIANS
         [0.0 1.0 0.0]))))

(deftest computing-θ
  (is (= (/ Math/PI 2.0) (#'core/compute-θ 0.0)))
  (is (= (/ Math/PI 4.0) (#'core/compute-θ 45.0))))

(deftest computing-ϕ
  (is (= 0.0 (#'core/compute-ϕ 0.0 (t/date-time 2024))))
  (is (= (- (/ Math/PI 2)) (#'core/compute-ϕ 0.0 (t/date-time 2024 1 1 6)))))

(deftest computing-compass-angles
  (is (= 0.0 (#'core/compass-angle 0.0 1.0)))
  (is (= 45.0 (#'core/compass-angle 1.0 1.0)))
  (is (= 90.0 (#'core/compass-angle 1.0 0.0)))
  (is (= 135.0 (#'core/compass-angle 1.0 -1.0)))
  (is (= 180.0 (#'core/compass-angle 0.0 -1.0)))
  (is (= 225.0 (#'core/compass-angle -1.0 -1.0)))
  (is (= 270.0 (#'core/compass-angle -1.0 0.0)))
  (is (= 315.0 (#'core/compass-angle -1.0 1.0))))

(deftest computing-inclination-angles
  (is (= 90.0 (#'core/inclination-angle 0.0 0.0 1.0)))
  (is (= 45.0 (#'core/inclination-angle 0.0 1.0 1.0)))
  (is (= 45.0 (#'core/inclination-angle 1.0 0.0 1.0)))
  (is (= 0.0 (#'core/inclination-angle 0.0 1.0 0.0)))
  (is (= -45.0 (#'core/inclination-angle 0.0 1.0 -1.0)))
  (is (= -90.0 (#'core/inclination-angle 0.0 0.0 -1.0))))

(deftest computing-the-apparent-position-of-the-sun
  ;; Midnight GMT, at equator on GMT longitude line. Sun would be straight down,
  ;; except for axial tilt.
  (is (= {:compass-angle 180.0
          :inclination-angle (- c/EARTH_AXIAL_TILT_DEGREES 90.0)}
         (#'core/apparent-position-of-sun 0.0 0.0 (t/date-time 2024))))
  (is (approximately-equal-maps?
        1.0E-6
        {:compass-angle (- 270.0 c/EARTH_AXIAL_TILT_DEGREES)
         :inclination-angle 0.0}
        (#'core/apparent-position-of-sun 0.0 -90.0 (t/date-time 2024)))))

(deftest computing-time-of-day
  (is (= "0:00:00" (#'core/compute-time-of-day-seconds 0.0 0 0.0)))
  (is (= "6:00:00" (#'core/compute-time-of-day-seconds 90.0 0 0.0)))
  (is (= "16:00:00" (#'core/compute-time-of-day-seconds 0.0 -8 0.0)))
  (is (= "12:00:00" (#'core/compute-time-of-day-seconds 0.0 0 Math/PI)))
  (is (= "10:00:00" (#'core/compute-time-of-day-seconds 90.0 -8 Math/PI))))

(deftest computing-sunrise-and-sunset
  (#'core/compute-sunrise-and-sunset 45.515 -122.678 -8 (t/date-time 2024 1 1))
  (#'core/compute-sunrise-and-sunset 45.515 -122.678 -8 (t/date-time 2024 2 1))
  (#'core/compute-sunrise-and-sunset 45.515 -122.678 -8 (t/date-time 2024 3 1))
  (#'core/compute-sunrise-and-sunset 45.515 -122.678 -8 (t/date-time 2024 4 1))
  (#'core/compute-sunrise-and-sunset 45.515 -122.678 -8 (t/date-time 2024 5 1))
  (#'core/compute-sunrise-and-sunset 45.515 -122.678 -8 (t/date-time 2024 6 1))
  (#'core/compute-sunrise-and-sunset 45.515 -122.678 -8 (t/date-time 2024 7 1))
  (#'core/compute-sunrise-and-sunset 45.515 -122.678 -8 (t/date-time 2024 8 1))
  (#'core/compute-sunrise-and-sunset 45.515 -122.678 -8 (t/date-time 2024 9 1))
  (#'core/compute-sunrise-and-sunset 45.515 -122.678 -8 (t/date-time 2024 10 1))
  (#'core/compute-sunrise-and-sunset 45.515 -122.678 -8 (t/date-time 2024 11 1))
  (#'core/compute-sunrise-and-sunset 45.515 -122.678 -8 (t/date-time 2024 12 1)))

(def portland-sunrise-and-sunset-data
  [{:longitude 45.515 :lattitude -122.678 :date (t/date-time 2024 1 1) :sunrise "7:50" :sunset "16:37"}
   {:longitude 45.515 :lattitude -122.678 :date (t/date-time 2024 2 1) :sunrise "7:32" :sunset "17:16"}
   {:longitude 45.515 :lattitude -122.678 :date (t/date-time 2024 3 1) :sunrise "6:47" :sunset "17:58"}
   {:longitude 45.515 :lattitude -122.678 :date (t/date-time 2024 4 1) :sunrise "6:49" :sunset "19:39"}
   {:longitude 45.515 :lattitude -122.678 :date (t/date-time 2024 5 1) :sunrise "5:57" :sunset "20:18"}
   {:longitude 45.515 :lattitude -122.678 :date (t/date-time 2024 6 1) :sunrise "5:25" :sunset "20:52"}
   {:longitude 45.515 :lattitude -122.678 :date (t/date-time 2024 7 1) :sunrise "5:26" :sunset "21:02"}
   {:longitude 45.515 :lattitude -122.678 :date (t/date-time 2024 8 1) :sunrise "5:55" :sunset "20:37"}
   {:longitude 45.515 :lattitude -122.678 :date (t/date-time 2024 9 1) :sunrise "6:32" :sunset "19:47"}
   {:longitude 45.515 :lattitude -122.678 :date (t/date-time 2024 10 1) :sunrise "7:09" :sunset "18:49"}
   {:longitude 45.515 :lattitude -122.678 :date (t/date-time 2024 11 1) :sunrise "7:51" :sunset "17:56"}
   {:longitude 45.515 :lattitude -122.678 :date (t/date-time 2024 12 1) :sunrise "7:31" :sunset "16:28"}])

(comment
  (require '[almanac.math :as math])
  (let [lattitude 45.5
        longitude -122.7
        time-zone-hours -8
        dt (t/date-time 2024 1 2)
        γ (#'core/earth-sun-angle dt)
        θ (#'core/compute-θ lattitude)
        α core/EARTH_AXIAL_TILT_RADIANS
        a (* (Math/cos γ) (Math/cos α) (Math/sin θ))
        b (* (Math/sin γ) (Math/sin θ))
        c (* (Math/cos γ) (Math/sin α) (Math/cos θ))]
    (->> (math/solve-sine-cosine-equation a b c)
         ;;(map (partial compute-time-of-day-seconds longitude time-zone-hours))
         ;;(sort)
         ;;(map format-time)
         )))
