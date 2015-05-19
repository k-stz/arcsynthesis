(in-package #:arc-12)

;;------------------------------------------------------------------------------
;;CFFI approach to light-block struct

;; first we need the glm::vec4 which shall be just an array of floats
(cffi:defcstruct per-light
  (camera-space-light-pos :float :count 4)
  (light-intensity :float :count 4))
 
;; from cffi doc:
(cffi:defcstruct light-block
  (ambient-intensity :float :count 4)
  (light-attenuation :float)
  ;; TODO: use :offet?
  (padding :float :count 3)
  ;; TODO: :count doesn't accept a +constant+ value, cffi bug?
  (lights (:struct per-light) :count 4))

;; allocating a c-struct:
(defparameter *lb*
  (cffi:foreign-alloc '(:struct light-block)))

;; since *lb* memory layout is practically a float array of 40 indices this
;; oughta work:
;; (dotimes (i 40)
;;   (setf
;;    (cffi:mem-aref *lb* :float i) 0.5)) ; yep, totally works!!

