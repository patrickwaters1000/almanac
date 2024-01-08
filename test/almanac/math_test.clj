(ns almanac.math-test
  (:require
    [almanac.math :as math]
    [almanac.testlib :refer [approx?
                             approximately-equal-vectors?]]
    [almanac.units :as u]
    [clojure.test :refer [is deftest are]]))

(deftest computing-dot-products
  (is (= 11.0 (math/dot [1 2] [3 4]))))

(deftest multiplying-matrices-and-vectors
  (is (= [7.0 15.0]
         (math/matrix-vector-product [[1 2] [3 4]]
                                     [1 3]))))

(deftest computing-angles
  (is (= 0.0 (u/radians-to-degrees (math/angle 1.0 0.0))))
  (is (= 45.0 (u/radians-to-degrees (math/angle 1.0 1.0))))
  (is (= (float 60.0)
         (float (u/radians-to-degrees
                  (math/angle 0.5
                              (/ (Math/sqrt 3.0) 2))))))
  (is (= 90.0 (u/radians-to-degrees (math/angle 0.0 1.0))))
  (is (= -90.0 (u/radians-to-degrees (math/angle 0.0 -1.0))))
  (is (= -45.0 (u/radians-to-degrees (math/angle 1.0 -1.0))))
  (is (= 180.0 (u/radians-to-degrees (math/angle -1.0 0.0))))
  (is (= 135.0 (u/radians-to-degrees (math/angle -1.0 1.0))))
  (is (= 225.0 (u/radians-to-degrees (math/angle -1.0 -1.0)))))

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

(deftest solving-quadratics
  (is (= [(- 1 (Math/sqrt 2))
          (+ 1 (Math/sqrt 2))]
         (math/solve-quadratic 1.0 -2.0 -1.0))))

(deftest solving-increasing-functions
  (is (approx? 1.0E-6
               (Math/sqrt 3.0)
               (math/solve-increasing-function
                 #(- (Math/pow % 2) 3.0)
                 1.0E-8
                 0.0
                 2.0))))
