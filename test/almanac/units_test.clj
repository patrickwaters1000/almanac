(ns almanac.units-test
  (:require
    [almanac.units :as u]
    [clj-time.core :as t]
    [clojure.test :refer [is deftest]]))

(deftest representing-dates-in-years
  (is (< 1.0
         (u/date-to-years (t/date-time 2025))
         1.003)))
