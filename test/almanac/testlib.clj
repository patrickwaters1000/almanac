(ns almanac.testlib
  (:require
    [clojure.test :refer [is deftest]]))

(defn approx? [tolerance x y]
  (>= tolerance (Math/abs (- x y))))

(defn approximately-equal-vectors? [tolerance v1 v2]
  (and (vector? v1)
       (vector? v2)
       (= (count v1) (count v2))
       (let [oks (map (partial approx? tolerance) v1 v2)]
         (every? identity oks))))

(defn approximately-equal-matrices? [tolerance m1 m2]
  (and (vector? m1)
       (vector? m2)
       (= (count m1) (count m2))
       (let [oks (map #(approximately-equal-vectors? tolerance %1 %2) m1 m2)]
         (every? identity oks))))

(defn approximately-equal-maps? [tolerance m1 m2]
  (and (map? m1)
       (map? m2)
       (= (set (keys m1))
          (set (keys m2)))
       (every? #(approx? tolerance
                         (get m1 %)
                         (get m2 %))
               (keys m1))))

(deftest checking-whether-vectors-are-approximately-equal
  (is (approximately-equal-vectors? 0.2 [1 2 3] [1.1 2.1 3.1]))
  (is (not (approximately-equal-vectors? 0.02 [1 2 3] [1.1 2.1 3.1]))))

(deftest checking-whether-matrices-are-approximately-equal
  (is (approximately-equal-matrices? 0.2 [[1 2] [3 4]] [[1.1 2] [3 4]]))
  (is (not (approximately-equal-matrices? 0.02 [[1 2] [3 4]] [[1.1 2] [3 4]]))))

(deftest checking-whether-maps-are-approximately-equal
  (is (approximately-equal-maps? 0.1
                                 {:a 1.0 :b 2.0}
                                 {:a 1.0 :b 2.01}))
  (is (not (approximately-equal-maps? 0.01
                                      {:a 1.0 :b 2.0}
                                      {:a 1.0 :b 2.1})))
  (is (not (approximately-equal-maps? 0.1
                                      {:a 1.0 :b 2.0}
                                      {:b 2.0}))))
