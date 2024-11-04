;; filepath: /Users/aleksejlapin/ITMO/Functional_Programming3/test/interpolation_test.clj

(ns interpolation-test
  (:require [clojure.test :refer [deftest is run-tests testing]]
            [interpolation :refer [->Point execute generate-steps lagrange-interpolation linear-interpolation]]))

(deftest linear-interpolation-test
  (testing "Linear interpolation between two points"
    (let [points [(->Point 0.0 0.0)
                  (->Point 2.0 2.0)]
          x      1.0
          expected 1.0
          result  (linear-interpolation points x)]
      (is (= expected result)))))

(deftest lagrange-interpolation-test
  (testing "Lagrange interpolation with four points"
    (let [points [(->Point 0.0 1.0)
                  (->Point 1.0 2.0)
                  (->Point 2.0 3.0)
                  (->Point 3.0 4.0)]
          x      1.5
          expected 2.5 ;; For evenly spaced points, Lagrange should align with linear
          result  (lagrange-interpolation points x)]
      (is (== expected (double (/ (Math/floor (* 100 result)) 100.0)))))))

(deftest generate-steps-test
  (testing "Generating steps from x-min to x-max with step size"
    (let [x-min 0.0
          x-max 3.0
          step   1.0
          expected [0.0 1.0 2.0 3.0]
          result  (generate-steps x-min x-max step)]
      (is (= expected result)))))

(deftest execute-linear-test
  (testing "Execute linear interpolation over points"
    (let [points_seq [(->Point 0.0 0.0)
                      (->Point 1.0 1.0)
                      (->Point 2.0 2.0)]
          step       1.0
          win-size   2
          expected   [(->Point 0.0 0.0)
                      (->Point 1.0 1.0)
                      (->Point 1.0 1.0)
                      (->Point 2.0 2.0)]
          result      (execute linear-interpolation points_seq step win-size)]
      (is (= expected (vec (flatten result)))))))

(deftest execute-lagrange-test
  (testing "Execute Lagrange interpolation over points"
    (let [points_seq [(->Point 0.0 1.0)
                      (->Point 1.0 2.0)
                      (->Point 2.0 3.0)
                      (->Point 3.0 4.0)]
          step       0.5
          win-size   4
          expected   [(->Point 0.0 1.0)
                      (->Point 0.5 1.5)
                      (->Point 1.0 2.0)
                      (->Point 1.5 2.5)
                      (->Point 2.0 3.0)
                      (->Point 2.5 3.5)
                      (->Point 3.0 4.0)]
          result      (execute lagrange-interpolation points_seq step win-size)]
      (is (= (list expected) result)))))
(run-tests)