(ns cs373.test.unit1.core
  (:use [cs373.unit1.core])
  (:use [clojure.test]))

(comment
  "Unit Tests for cs373.unit1.core/move function.")

(def small-world [0 1 0 0 0])

(deftest one-step-back
  (is (= [1 0 0 0 0] (move small-world -1))))

(deftest one-step-forth
  (is (= [0 0 1 0 0] (move small-world 1))))

(deftest one-lap-turns-to-start-point
  (is (= small-world (move small-world 5))))

(deftest no-move
  (is (= small-world (move small-world 0))))