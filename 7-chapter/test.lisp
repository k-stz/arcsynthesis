;; TODO: solve this mystery!

(defpackage test
  (:use :cl))

(in-package test)

;; why does foo get the value: cl::VAL, instead of just VAL?
(defparameter foo 0)
(print *package*)
(defun foo ()
  (print *package*)
    (setf foo (read-from-string "val")))


(bt:make-thread #'foo)
;; (bt:make-thread #'foo :name (acons '*package* *package*
;;       bt:*default-special-bindings*))

