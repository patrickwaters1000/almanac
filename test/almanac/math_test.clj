(ns almanac.math-test
  (:require
    [almanac.math :as math]
    [almanac.testlib :refer [approximately-equal-vectors?]]
    [clojure.test :refer [is deftest are]]))

(deftest computing-dot-products
  (is (= 11.0 (math/dot [1 2] [3 4]))))

(deftest multiplying-matrices-and-vectors
  (is (= [7.0 15.0]
         (math/matrix-vector-product [[1 2] [3 4]]
                                     [1 3]))))

(deftest computing-angles
  (is (= 0.0 (math/radians-to-degrees (math/angle 1.0 0.0))))
  (is (= 45.0 (math/radians-to-degrees (math/angle 1.0 1.0))))
  (is (= (float 60.0)
         (float (math/radians-to-degrees
                  (math/angle 0.5
                              (/ (Math/sqrt 3.0) 2))))))
  (is (= 90.0 (math/radians-to-degrees (math/angle 0.0 1.0))))
  (is (= -90.0 (math/radians-to-degrees (math/angle 0.0 -1.0))))
  (is (= -45.0 (math/radians-to-degrees (math/angle 1.0 -1.0))))
  (is (= 180.0 (math/radians-to-degrees (math/angle -1.0 0.0))))
  (is (= 135.0 (math/radians-to-degrees (math/angle -1.0 1.0))))
  (is (= 225.0 (math/radians-to-degrees (math/angle -1.0 -1.0)))))

(deftest solving-sine-cosine-equations
  (are [a b c num-solutions]
    (approximately-equal-vectors?
      1.0E-6
      (vec (repeat num-solutions 0))
      (mapv #(+ (* a (Math/cos %))
                (* b (Math/sin %))
                c)
            (math/solve-sine-cosine-equation a b c)))
    4.0 5.0 6.0 2
    0.0 1.0 -0.5 2
    1.0 1.0 (- (Math/sqrt 2.0)) 1))
