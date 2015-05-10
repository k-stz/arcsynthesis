;; TODO: is this an acceptable file organization principle for cl?

(in-package #:arc-12)

(defclass program-data ()
  ((the-program :accessor the-program)

   (model-to-camera-matrix-unif :accessor model-to-camera-matrix-unif)
   (normal-model-to-camera-matrix-unif :accessor normal-model-to-camera-matrix-unif)))

