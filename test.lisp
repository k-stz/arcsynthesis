;; illumination only

(defpackage "TEST"
  (:use :cl))

(in-package "TEST")

(defun test (x)
  (declare (number x))
  (print x))

