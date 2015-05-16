(in-package #:arc-12)

;; *lights* LightManager

;; (defclass light-manager () ()
;;   )

(defclass per-light ()
  ((camera-space-light-pos :initform (glm:vec4 0.0)) ; vec4
   (light-intensity :initform (glm:vec4 0.5))))	     ; vec4

(defconstant +number-of-lights+ 4)

(defclass light-block ()
  ((ambient-intensity :initform (glm:vec4 .25) :accessor ambient-intensity) ; vec4
   (light-attenuation :initform 1.0 :accessor light-attenuation)	    ; float
   ;; "padding 3" is taken care of in the AS-GLARR method!
   (lights :initform
	   (loop for i below +number-of-lights+
	      :collect (make-instance 'per-light))
	   :accessor lights)))

;; use gl::array-byte-size for quick tests
(defgeneric as-glarr (obj)
  (:documentation "Return a gl-array representation of the object."))

;; this will be an ugly hack setting just magic-number the padding[3]
(defmethod as-glarr ((lb light-block))
  (let (;;light-block
	(ai (ambient-intensity lb))
	(la (vector (light-attenuation lb)))
	;; padding[3]:
	(padding (make-array 3 :element-type 'single-float))
	;;per-light
	(lights
	 (apply #'concatenate 'vector
		(loop for per-light-obj in (lights lb) collect
		     (concatenate 'vector
				  (slot-value per-light-obj 'camera-space-light-pos)
				  (slot-value per-light-obj 'light-intensity)))))
	(data))
    (setf data (concatenate 'vector
			    ai la padding lights))
    ;; data stored. building the gl-array:
    (arc:create-gl-array-from-vector data)))

