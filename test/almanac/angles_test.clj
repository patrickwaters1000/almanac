(ns almanac.angles-test
  (:require
    [almanac.angles :as a]
    [almanac.constants :as c]
    [almanac.testlib :refer [approx?]]
    [clojure.test :refer [is deftest]]))

(deftest computing-polar-angle
  (is (approx? 1.0E-6 (a/polar-angle 90.0) 0.0))
  (is (approx? 1.0E-6 (a/polar-angle 45.0) (/ Math/PI 4.0)))
  (is (approx? 1.0E-6 (a/polar-angle 0.0) (/ Math/PI 2.0)))
  (is (approx? 1.0E-6 (a/polar-angle -45.0) (* 0.75 Math/PI))))

(deftest computing-compass-angles
  (is (= 0.0 (a/compass-angle 0.0 1.0)))
  (is (= 45.0 (a/compass-angle 1.0 1.0)))
  (is (= 90.0 (a/compass-angle 1.0 0.0)))
  (is (= 135.0 (a/compass-angle 1.0 -1.0)))
  (is (= 180.0 (a/compass-angle 0.0 -1.0)))
  (is (= 225.0 (a/compass-angle -1.0 -1.0)))
  (is (= 270.0 (a/compass-angle -1.0 0.0)))
  (is (= 315.0 (a/compass-angle -1.0 1.0))))

(deftest computing-inclination-angles
  (is (= 90.0 (a/inclination-angle 0.0 0.0 1.0)))
  (is (= 45.0 (a/inclination-angle 0.0 1.0 1.0)))
  (is (= 45.0 (a/inclination-angle 1.0 0.0 1.0)))
  (is (= 0.0 (a/inclination-angle 0.0 1.0 0.0)))
  (is (= -45.0 (a/inclination-angle 0.0 1.0 -1.0)))
  (is (= -90.0 (a/inclination-angle 0.0 0.0 -1.0))))

;;(deftest computing-θ
;;  (is (= (/ Math/PI 2.0) (#'core/compute-θ 0.0)))
;;  (is (= (/ Math/PI 4.0) (#'core/compute-θ 45.0))))
;;
;;(deftest computing-ϕ
;;  (is (= 0.0 (#'core/compute-ϕ 0.0 (t/date-time 2024))))
;;  (is (= (- (/ Math/PI 2)) (#'core/compute-ϕ 0.0 (t/date-time 2024 1 1 6)))))
