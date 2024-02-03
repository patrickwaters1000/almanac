(ns almanac.sunrise-equation-test
  (:require
    [almanac.constants :as c]
    [almanac.sunrise-equation :as eq]
    [almanac.testlib :refer [approx?
                             approximately-equal-vectors?]]
    [clj-time.core :as t]
    [clojure.test :refer [is deftest]]))

(deftest solving-sunrise-equation
  (doseq [{:keys [tolerance θ γ want]}
            [{:tolerance 0.01
              :θ (/ Math/PI 4.0)
              :γ (/ Math/PI 3.0)
              :want [2.88 -0.715]}]
          :let [got (eq/solve-for-earth-rotational-angle θ γ)]]
    (is (approximately-equal-vectors? tolerance want got))))

(deftest computing-datetime-for-earth-orbital-angle
  (let [dt1 (t/date-time 2024 3 22)
        dt2 (#'eq/compute-datetime-for-earth-rotational-angle 0 dt1 Math/PI)
        seconds-got (t/in-seconds (t/interval dt1 dt2))
        seconds-want (t/in-seconds (t/hours 6))]
    (is (approx? 30 seconds-got seconds-got))))
