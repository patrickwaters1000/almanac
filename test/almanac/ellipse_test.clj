(ns almanac.ellipse-test
  (:require
    [almanac.ellipse :as ellipse]
    [almanac.testlib :refer [approx?]]
    [clojure.test :refer [is deftest]]))

(deftest calculating-area-under-ellipse
  (is (< -0.5 (#'ellipse/area-under-ellipse 1.0 -0.5) -0.45))
  (is (< 0.45 (#'ellipse/area-under-ellipse 1.0 0.5) 0.5))
  (is (= (* 2 (#'ellipse/area-under-ellipse 1.0 0.5))
         (#'ellipse/area-under-ellipse 2.0 1.0))))

(deftest calculating-elliptic-sector-areas
  (is (zero? (ellipse/elliptic-sector-area 1.0 0.0)))
  (is (approx? 1.0E-8
               (/ Math/PI 4)
               (ellipse/elliptic-sector-area 1.0 (/ Math/PI 2))))
  (is (approx? 1.0E-8
               (/ Math/PI 2)
               (ellipse/elliptic-sector-area 1.0 Math/PI)))
  (is (approx? 1.0E-8
               (- (/ Math/PI 4))
               (ellipse/elliptic-sector-area 1.0 (- (/ Math/PI 2)))))
  (is (approx? 1.0E-8
               (* 1.25 Math/PI)
               (ellipse/elliptic-sector-area 1.0 (* 2.5 Math/PI)))))

;; TODO Figure out why this fails for t=0.
(for [t (range 0.01 (* 2 Math/PI) 0.1)]
  {:circular-angle t
   :elliptic-angle (ellipse/orbital-angle
                     0.02 0.0 t)})
