(ns almanac.sunrise-equation-test
  (:require
    [almanac.sunrise-equation :as eq]
    [clojure.test :refer [is deftest]]))

(deftest solving-sunrise-equation
  (is (= [2.8847983136588824 -0.7155198889385804]
         (eq/solve-for-ϕ (/ Math/PI 4.0) (/ Math/PI 3.0)))))
