(defpackage cl-pyim/tests/main
  (:use :cl
        :cl-pyim
        :rove))
(in-package :cl-pyim/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-pyim)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
	   (ok (= 1 1))))

