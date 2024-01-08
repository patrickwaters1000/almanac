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

(def portland-lattitude 45.515)
(def portland-longitude -122.678)
(def portland-sunrise-and-sunset-data
  [[(t/date-time 2024 1 1) -8 "7:50" "16:37"]
   [(t/date-time 2024 2 1) -8 "7:32" "17:16"]
   [(t/date-time 2024 3 1) -8 "6:47" "17:58"]
   [(t/date-time 2024 4 1) -7 "6:49" "19:39"]
   [(t/date-time 2024 5 1) -7 "5:57" "20:18"]
   [(t/date-time 2024 6 1) -7 "5:25" "20:52"]
   [(t/date-time 2024 7 1) -7 "5:26" "21:02"]
   [(t/date-time 2024 8 1) -7 "5:55" "20:37"]
   [(t/date-time 2024 9 1) -7 "6:32" "19:47"]
   [(t/date-time 2024 10 1) -7 "7:09" "18:49"]
   [(t/date-time 2024 11 1) -7 "7:51" "17:56"]
   [(t/date-time 2024 12 1) -8 "7:31" "16:28"]])

(defn hms-str->minutes
  "Parses a string of either form h:mm:ss, hh:mm:ss, h:mm or hh:mm as a float
  valued number of minutes since midnight."
  [hms-str]
  (let [regex #"(\d{1,2}):(\d{2,2})(:(\d{2,2}))?"
        [_ h-str m-str _ s-str] (re-matches regex hms-str)
        h (Integer/parseInt h-str)
        m (Integer/parseInt m-str)
        s (if s-str
            (Integer/parseInt s-str)
            0)]
    (+ (* 60.0 h) m (/ s 60.0))))

(defn compute-errors [& {:keys [coarse-sunrise-eq circular-orbit]}]
  (for [[date
         time-zone
         sunrise-want-str
         sunset-want-str] portland-sunrise-and-sunset-data
        :let [config (core/map->Config
                       {:lattitude portland-lattitude
                        :longitude portland-longitude
                        :time-zone time-zone
                        :date date
                        :coarse-sunrise-eq coarse-sunrise-eq
                        :circular-orbit circular-orbit})
              [sunrise-got-str
               sunset-got-str] (#'core/compute-sunrise-and-sunset config)
              [sunrise-got
               sunset-got] (->> [sunrise-got-str
                                 sunset-got-str]
                                (map hms-str->minutes))
              [sunrise-want
               sunset-want] (map hms-str->minutes [sunrise-want-str
                                                   sunset-want-str])
              sunrise-error (- sunrise-got sunrise-want)
              sunset-error (- sunset-got sunset-want)
              day-length-error (- sunset-error sunrise-error)]]
    {:date (subs (str date) 0 10)
     :sunrise-got sunrise-got-str
     :sunrise-want sunrise-want-str
     :sunset-got sunset-got-str
     :sunset-want sunset-want-str
     :sunrise-error sunrise-error
     :sunset-error sunset-error
     :day-length-error day-length-error}))

(defn mean-absolute-value [xs]
  (let [n (count xs)
        abs-err (->> xs
                     (map #(Math/abs %))
                     (reduce + 0.0))]
    (/ abs-err n)))

(defn compute-mae [& {:keys [coarse-sunrise-eq
                             circular-orbit]}]
  (let [errors (compute-errors :coarse-sunrise-eq coarse-sunrise-eq
                               :circular-orbit circular-orbit)]
    {:sunrise-error (mean-absolute-value (map :sunrise-error errors))
     :sunset-error (mean-absolute-value (map :sunset-error errors))
     :day-length-error (mean-absolute-value (map :day-length-error errors))}))

(comment
  (compute-errors :coarse-sunrise-eq true
                  :circular-orbit false)
  (for [coarse-sunrise-eq [false true]
        circular-orbit [false true]]
    (assoc (compute-mae :coarse-sunrise-eq coarse-sunrise-eq
                        :circular-orbit circular-orbit)
      :coarse-sunrise-eq coarse-sunrise-eq
      :circular-orbit circular-orbit))
  ;;
  )
