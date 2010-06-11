(ns turing-machines.core-test
  (:use [turing-machines.core :as tm :only []] :reload-all)
  (:use [clojure.test]))

(deftest test-sum-runs
  (is (= (tm/final-number
          (apply
           (tm/compile-turing-program
            [0 true  true  :right 0]
            [0 false true  :right 1]
            [1 true  true  :right 1]
            [1 false false :left  2]
            [2 true  false :left  3]
            [3 true  true  :left  3]
            [3 false false :right 4])
           (tm/make-tape [true true true false true true])))
         5)))
