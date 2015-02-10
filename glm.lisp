;;TODO: how is sb-cga pretty printing its matrix ?
;;     (deftype matrix (simple-array ..)) then implement print-object
;;     function on it??

;; This code tries to emulate the GLM-library
;; TODO: readermacro? *matrix*0.x => (mat4-place *matrix* 0 :x)

;; TODO: are those macros really properly utilized here?

(in-package #:glm)

;;first a simple solution TODO: just use sb-cga functions internally, like glm:vec-
(defun make-mat4 (init-diagonal-values)
  (let ((idv init-diagonal-values))	;bad style?
    (make-array 16 :element-type 'single-float
		:initial-contents
		(list idv 0.0 0.0 0.0              
		      0.0 idv 0.0 0.0    
		      0.0 0.0 idv 0.0
		      0.0 0.0 0.0 idv))))

;; TODO: 
;; is +notation+ acceptable? Can't be constant for DEFCONSTANT gets its value at
;; compile-time while DEFUN work at toplevel
;; TODO: compile-time, top-level :execute intuitive understanding: start at EVAL-WHEN clhs
(defparameter +identity-mat4+ (make-mat4 1.0)) ;;TODO: make me a constant


;; so as to be used in conjunction with SETF
;; TODO: maybe SETF facilitates this somehow already? some DEFMETHOD?
(defmacro mat4-place (mat4 row coordinate)
  (let ((c (ecase coordinate
	    (:x 0) (:y 1) (:z 2) (:w 3))))
    `(aref ,mat4 ,(+ row (* 4 c)))))

(defmacro mat4-col-place (mat4 col coordinate)
  (let ((c (ecase coordinate
	     (:x 0) (:y 1) (:z 2) (:w 3))))
    `(aref ,mat4 ,(+ c (* col 4)))))

(defmacro set-mat4 (mat4 col col-coordinate set-value)
  ;; wow, this setf doesn't work unless mat4-place is a macro expanding
  ;; an AREF
  "Set specific place in given matrix"
  `(setf (mat4-col-place ,mat4 ,col ,col-coordinate) ,set-value))


;; this can't work as a macro, because a macro can't have runtime
;; specific information. It can't know the length of the <vector>
;; given because it is created at runtime (example: (init-g-instance-list)
;;
;; TODO: shouldn't try to implement glm, as it seems to get most of its
;; features from function overloading. Abandon this approach altogether?
;; (defun set-mat4-row-any-vec (mat4 row vector)
;;   (let ((key (ecase row
;; 	       (0 :x) (1 :y) (2 :z) (3 :w))))
;;     (loop for vector-element across vector
;;        for j = 0 then (1+ j)
;;        do
;; 	 `(setf (mat4-place ,mat4 j ,key) vector-element)
;; 	 )))

;; noooooo, this is alllll wrooooooong. gl:uniform-matrix transposes its input matrix by default,
;; hence I've tested this function based on the visual representation being right -.-
(defmacro set-mat4-col (mat4 col vec4)
  (let ((key (ecase col
	       (0 :x) (1 :y) (2 :z) (3 :w))))
    `(progn (setf (mat4-place ,mat4 0 ,key) (aref ,vec4 0))
	    (setf (mat4-place ,mat4 1 ,key) (aref ,vec4 1))
	    (setf (mat4-place ,mat4 2 ,key) (aref ,vec4 2))
	    (setf (mat4-place ,mat4 3 ,key) (aref ,vec4 3))))
  )

;; TODO: (set-mat4-row *m* 3 #(1.0 2.0 3.0)) doesn't work: #(..) has to be (vector ..)
;; why?
(defmacro set-mat4-row (mat4 row vec4)
  `(progn (setf (mat4-place ,mat4 ,row :x) (aref ,vec4 0))
	  (setf (mat4-place ,mat4 ,row :y) (aref ,vec4 1))
	  (setf (mat4-place ,mat4 ,row :z) (aref ,vec4 2))
	  (setf (mat4-place ,mat4 ,row :w) (aref ,vec4 3)))
  )


(defmacro set-mat4-diagonal (mat4 vec4)
  `(progn (setf (mat4-place ,mat4 0 :x) (aref ,vec4 0))
	  (setf (mat4-place ,mat4 1 :y) (aref ,vec4 1)) 
	  (setf (mat4-place ,mat4 2 :z) (aref ,vec4 2)) 
	  (setf (mat4-place ,mat4 3 :w) (aref ,vec4 3))))
;; interesting having the progn of the above return mat4, would just expand into an
;; expression returning a "copy" of mat4 as if premutations haven't occured


;; to facilitate pure vector negation 
(defun vec- (a &optional b)
  (if (null b)
      ;; TODO: this doesn't look right, but compiler probably smart enough?
      (sb-cga:vec- a (sb-cga:vec* a 2.0))
      (sb-cga:vec- a b)))

 (defmacro vec. (vector xyzw)
   ;; TODO: isn't it "SETFable?"
  "AREFable vector component returned. In the vein of C++ notation: vec.x vec.y etc."
  `(aref ,vector
	 (case ,xyzw
		(:x 0) (:y 1) (:z 2) (:w 3)
		(t (format *error-output* "VEC. bad input:~a ;defaulting to 0!!" ,xyzw)
		   0))))

(defun vec3 (x &optional (y x) (z x))
  (let ((x (float x))
	(y (float y))
	(z (float z)))
    (make-array 3 :element-type 'single-float
		:initial-contents (list x y z))))

(defun vec4 (x &optional (y x) (z x) (w x))
  (let ((x (float x))
	(y (float y))
	(z (float z))
	(w (float w)))
    (make-array 4 :element-type 'single-float
		:initial-contents (list x y z w))))

(defun normalize (vec)
  (let ((length))
    ;; get vector length using pythagoras
    (setf length
	  (sqrt
	   (apply #'+ (loop for i across vec
			 collect
			   (expt i 2)))))

    ;; LOOP's starting to aid me intuitively
    (loop for element across vec
       and i from 0
	 do
	 (setf (aref vec i) (/ element length)))
    vec))


(defun vec4-from-vec3 (vec3 &optional w)
  "Fills vec4 with vec3 ending in w:1.0!"
  (let ((x (aref vec3 0))
	(y (aref vec3 1))
	(z (aref vec3 2))
	(w (if w w 1.0)))
    (make-array 4 :element-type 'single-float
		:initial-contents (list x y z w))))

;;simple implementation

(defun row-vec4-from-mat4 (row mat4)
  (let ((x row)
	(y (+ row 4))
	(z (+ row (* 4 2)))
	(w (+ row (* 4 3))))
    (vec4 (aref mat4 x )
	  (aref mat4 y )
	  (aref mat4 z )
	  (aref mat4 w ))))

(defmacro switch-mat4-row (row-1 row-2 mat4)
  `(let ((c1 (row-vec4-from-mat4 ,row-1 ,mat4))
	(c2 (row-vec4-from-mat4 ,row-2 ,mat4)))
    (set-mat4-row ,mat4 ,row-1 c2)
    (set-mat4-row ,mat4 ,row-2 c1)
    ))

(defun make-mat3 (init-diagonal-values)
  (let ((idv init-diagonal-values))
    (make-array 9 :element-type 'single-float
		:initial-contents
		(list idv 0.0 0.0
		      0.0 idv 0.0    
		      0.0 0.0 idv))))

(defun mat4-from-mat3 (mat3)
  "Put mat3 into top-left corner of an identity mat4"
  (make-array 16 :element-type 'single-float
	      :initial-contents
	      (append 
	       (loop for i across mat3
		  for x = 1 then (1+ x)
		  if (= (mod x 3) 0)
		  collect i and collect 0.0
		  else collect i)
	       '(0.0 0.0 0.0 1.0))))

(defmacro mat3-place (mat3 row coordinate)
  (let ((c (ecase coordinate
	    (:x 0) (:y 1) (:z 2))))
    `(aref ,mat3 ,(+ (* 3 row)  c))))

(defmacro set-mat3 (mat3 row coordinate set-value)
  ;; wow, this setf doesn't work unless mat4-place is a macro expanding
  ;; an AREF
  `(setf (mat3-place ,mat3 ,row ,coordinate) ,set-value))


;;; transformations:
(defun ang-rad-from-ang-deg (ang-deg)
  (let ((rad-deg-ratio (/ (float pi 1.s0) 180.0)))
    (* rad-deg-ratio ang-deg)))

(defun rotate-x (ang-deg)
  (let* ((ang-rad (ang-rad-from-ang-deg ang-deg))
	 (f-cos (cos ang-rad))
	 (f-sin (sin ang-rad))
	 (matrix (glm:make-mat3 1.0)))
    (glm:set-mat3 matrix 1 :y f-cos) (glm:set-mat3 matrix 2 :y (- f-sin)) 
    (glm:set-mat3 matrix 1 :z f-sin) (glm:set-mat3 matrix 2 :z f-cos)
    (glm:mat4-from-mat3 matrix)
    ))

;;TODO: all these function probably need some sensible rounding so as that
;; applying two times a 180 degree turn would result in the old position
(defun rotate-y (ang-deg)
  (let* ((ang-rad (ang-rad-from-ang-deg ang-deg))
	 (f-cos (cos ang-rad))
	 (f-sin (sin ang-rad))
	 (matrix (glm:make-mat3 1.0))
	 )
    ;; since even this function calculates rotation unlike arcsynthesis I will leave
    ;; this function the way it is (arcsynthesis rotates counter-clockwise with positive
    ;; angles given, positve axis pointing into eye)
    ;; (sb-cga:rotate-around (sb-cga:vec 0.0 1.0 0.0) ang-rad)
    (glm:set-mat3 matrix 0 :x f-cos)     (glm:set-mat3 matrix 2 :x f-sin) 
    (glm:set-mat3 matrix 0 :z (- f-sin)) (glm:set-mat3 matrix 2 :z f-cos)
    (glm:mat4-from-mat3 matrix)
    ))

(defun rotate-z (ang-deg)
  (let* ((ang-rad (ang-rad-from-ang-deg ang-deg))
	 (f-cos (cos ang-rad))
	 (f-sin (sin ang-rad))
	 (matrix (glm:make-mat3 1.0)))
    (glm:set-mat3 matrix 0 :x f-cos) (glm:set-mat3 matrix 1 :x (- f-sin)) 
    (glm:set-mat3 matrix 0 :y f-sin) (glm:set-mat3 matrix 1 :y f-cos)
    (glm:mat4-from-mat3 matrix)
    ))


(defun rotate-axis (axis-x axis-y axis-z ang-deg)
  (let* ((ang-rad (ang-rad-from-ang-deg ang-deg))
	 (f-cos (cos ang-rad))
	 (f-inv-cos (- 1.0 f-cos))
	 (f-sin (sin ang-rad))
;	 (f-inv-sin (- 1.0 f-sin))

	 (axis (glm:vec3 axis-x axis-y axis-z))
	 ;; Oh, wow it needs to be normalized.
	 ;; I assume the vector then must be changing length?
	 (axis (glm:normalize axis))
	 (a-x (aref axis 0)) (a-y (aref axis 1)) (a-z (aref axis 2))
	 (matrix (glm:make-mat3 1.0)))
    (glm:set-mat3 matrix 0 :x (+ (* a-x a-x) (* (- 1 (* a-x a-x)) f-cos)))
    (glm:set-mat3 matrix 1 :x (- (* a-x a-y f-inv-cos) (* a-z f-sin)))
    (glm:set-mat3 matrix 2 :x (+ (* a-x a-z f-inv-cos) (* a-y f-sin)))

    (glm:set-mat3 matrix 0 :y (+ (* a-x a-y f-inv-cos) (* a-z f-sin)))
    (glm:set-mat3 matrix 1 :y (+ (* a-y a-y) (* (- 1 (* a-y a-y)) f-cos)))
    (glm:set-mat3 matrix 2 :y (- (* a-y a-z f-inv-cos) (* a-x f-sin)))

    (glm:set-mat3 matrix 0 :z (- (* a-x a-z f-inv-cos) (* a-y f-sin)))
    (glm:set-mat3 matrix 1 :z (+ (* a-y a-z f-inv-cos) (* a-x f-sin)))
    (glm:set-mat3 matrix 2 :z (+ (* a-z a-z) (* (- 1 (* a-z a-z)) f-cos)))

    (glm:mat4-from-mat3 matrix)
    ))



;;;TODO: more macro experiments needed, what do with nested macros?
;; (defun col-vec4-from-mat4 (col mat4)
;;   `(let ((key (ecase ,col
;; 	       (0 :x) (1 :y) (2 :z) (3 :w))))
;;     (vec4 (mat4-place mat4 0 key)
;; 	   (mat4-place mat4 1 key)
;; 	   (mat4-place mat4 2 key)
;; 	   (mat4-place mat4 3 key)
;; 	  )))

 ;; (defun switch-mat4-col (col-1 col-2 mat4)
 ;;   (let ((c1 (col-vec4-from-mat4 col-1 mat4)))
 ;;     (print c1))

 ;;   )

;; (defmacro foo (col mat4)
;;   `(let ((key (ecase ,col
;; 	       (0 :x) (1 :y) (2 :z) (3 :w))))
;;     `(mat4-place ,,mat4 0 ,,key)))


(defun clamp (x min max)
  (if (< x min)
      min
      (if (> x max)
	  max
	  x)))


(defun mix (x y a)
  ;; from OpenGL description, probably this is only for a being [0,1]. Yep:
  ;; 'a' is the distance between x and y as if mapped to 0 to 1.0. The rest abides
  ;; to linear interpolation
  "linearly interpolate between two values x,y using 'a' to weight between them"
  (+ (* x (1- a))
     (* y a))
  )


;;Quaternions-------------------------------------------------------------------

;;TODO: change print representation to show the qt-vec4!
(defclass quat ()
  ((qt-vec4 :initform (glm:vec4 1.0) :initarg :vec4 :accessor qt-vec4)
   (qt-scalar :accessor qt-scalar) ; as radians
   (qt-axis-v3 :accessor qt-axis-v3)))


;; Initilize-instance :after works like a constructor in OOP languages. In the body of this function
;; we can access the instance created 'q' of class QUAT and set some values before unleashing
;; the object to the general public:
(defmethod initialize-instance :after ((q quat) &key &allow-other-keys)
  (setf (qt-scalar q) (vec. (qt-vec4 q) :x))
  (setf (qt-axis-v3 q) (vec3 (vec. (qt-vec4 q) :y)
			     (vec. (qt-vec4 q) :z)
			     (vec. (qt-vec4 q) :w))))

;;alas using a class to represent quaternions (useful as '*' can't be casted so we need
;;'quat*' for a quaternion multiplication procedure. Now because our quaternion is a class
;; we can't use NORMALIZE on it, and again need to create a new name for it:
;; Anyway this distinction may be favorable as quaternion normalization may be a bit different
;; as it only involves the normalization of the axis part of the quaternion (not the scalar)
;; TODO: confirm this
(defgeneric normalize-quat (quat))
(defmethod normalize-quat ((q quat))
  (setf (qt-axis-v3 q) (sb-cga:normalize (qt-axis-v3 q)))
  q)

(defmacro make-quat (angle-radians (axis-x axis-y axis-z))
  "Providing an angle in degree and an axis a quaternion is created and returned.
If the axis provided is of unit length the resulting quaternion will also be of
unit length"
  `(vec4->quat (glm:vec4 (float ,axis-x 1.0)
			 (float ,axis-y 1.0)
			 (float ,axis-z 1.0)
                         (float ,angle-radians 1.0))))


(defun vec4->quat (vec4)
  "Transform a 4D vector to get a quaternion. Input is treated as: 
 (theta axis-x axis-y axis-z). Where theta is treated as RADIAN. If 
the axis provided is already of unit length, the result is a _unit quaternion_."
  (let* ((theta (vec. vec4 :w))
	 (x (* (vec. vec4 :x) (sin (/ theta 2))))
	 (y (* (vec. vec4 :y) (sin (/ theta 2))))
	 (z (* (vec. vec4 :z) (sin (/ theta 2)))))
    (make-instance 'quat :vec4
		   ;;note the deviation from the form used in the book
		   (vec4 x
			 y
			 z
			 (cos (/ theta 2))
			 ))))

;; naming this just '*' as in providing operator overloading, is not possible
;; error will be singnaled: "'*' already names an ordinary function or a macro."
(defgeneric quat* (quat quat))
(defmethod quat* ((q1 quat) (q2 quat))
  "Quaternion multiplication"
  (let* ((qv1 (qt-vec4 q1)) (qv2 (qt-vec4 q2)))
    (multiple-value-bind ;(a.w a.x a.y a.z)
	  (a.x a.y a.z a.w)
	(values-list (loop for i across qv1 collecting i))
      (multiple-value-bind ;(b.w b.x b.y b.z)
	    (b.x b.y b.z b.w)
	  (values-list (loop for i across qv2 collecting i))
	;; Quaternion multiplication being a composition orientation the result is already
	;; a quaternion. Hence we directly make an instance without MAKE-QUATERNION
	(make-instance
	 'quat :vec4
	 (glm:vec4 (- (+ (* a.w b.x) (* a.x b.w) (* a.y b.z)) (* a.z b.y))
		   (- (+ (* a.w b.y) (* a.y b.w) (* a.z b.x)) (* a.x b.z))
		   (- (+ (* a.w b.z) (* a.z b.w) (* a.x b.y)) (* a.y b.x))
		   (-    (* a.w b.w) (* a.x b.x) (* a.y b.y)  (* a.z b.z))))))))



;; supposed to cast from multiple structures or types to a matrix, for now only
;; from quaternion
;; For intuitition: inspecting the (mat4-cast (make-quat 45.0 (0.0 1.0 0.0))), for example, shows
;; that quaternion perform a clock-wise rotation around the axis they represent (in direction of the axis)
(defgeneric mat4-cast (t))
(defmethod mat4-cast ((q1 quat))
  "Retruns the transformation matrix the input quaternion is representing"
  (multiple-value-bind (x y z w)
      (values-list (loop for i across (qt-vec4 q1) collecting i))
    (let ((mat4 (glm:make-mat4 1.0)))
      (glm:set-mat4-row mat4 0
			(glm:vec4 (- 1 (* 2 y y) (* 2 z z))
				  (- (* 2 x y) (* 2 w z))
				  (+ (* 2 x z) (* 2 w y)) 0))
      (glm:set-mat4-row mat4 1
			(glm:vec4 (+ (* 2 x y) (* 2 w z))
				  (- 1 (* 2 x x) (* 2 z z))
				  (- (* 2 y z) (* 2 w x)) 0))
      (glm:set-mat4-row mat4 2
			(glm:vec4 
			 (- (* 2 x z) (* 2 w y))
			 (+ (* 2 y z) (* 2 w x))
			 (- 1 (* 2 x x) (* 2 y y)) 0))
      mat4)))

;;Experimental------------------------------------------------------------------
;; TODO: experiment later using a class :I, maybe just use it to have a neat
;; print representation of the array (new-line every 4 values)?
(defclass mat4 ()
  ((mat4 :initarg :mat4-contents
	 :accessor get-matrix)))

(defun create-mat4 (init-diagonal-values)
  (let ((idv init-diagonal-values))	;bad style?
    (make-instance 'mat4 :mat4-contents
		   (make-array 16 :element-type 'single-float
			       :initial-contents
			       (list idv 0.0 0.0 0.0              
				     0.0 idv 0.0 0.0    
				     0.0 0.0 idv 0.0
				     0.0 0.0 0.0 idv)))))


(defmethod print-object ((matrix mat4) stream)
  (print-unreadable-object (matrix stream :type 'single-float :identity t)
    (format stream "~A" (get-matrix matrix))))
