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


(defun clamp (x min max)
  (if (< x min)
      min
      (if (> x max)
	  max
	  x)))

;; beware used by rotation.lisp and scale.lisp
;; -> use defmethod for vec4 alternative
(defun mix (x y a)
  ;; from OpenGL description, probably this is only for a being [0,1]. Yep:
  ;; 'a' is the distance between x and y as if mapped to 0 to 1.0. The rest abides
  ;; to linear interpolation
  "linearly interpolate between two values x,y using 'a' to weight between them"
  (+ (* x (1- a))
     (* y a))
  )


;;Quaternions-------------------------------------------------------------------

;; note this quaternion representation is not accurate in mathematics quaternions
;; seem to be represented by imaginary numbers. This representation, however, not
;; only suffices, it also provides a very simple model of a quaternion: a 4D vector
;; with a scalar and a axial part. This model seems, so far, to fully suffice for
;; the purpose of arcsynthesis, and maybe even for all of graphics programming.
(defclass quat ()
  ((w :initform 0.0 :initarg :w :accessor q.w)
   (x :initform 1.0 :initarg :x :accessor q.x)
   (y :initform 0.0 :initarg :y :accessor q.y)
   (z :initform 0.0 :initarg :z :accessor q.z)
   ))

;; nice, works! 
(defmethod print-object ((q quat) stream)
  ;; #<object> those #<> signal to the cl READer that these objects can't be read
  ;; back from their printed form, and the reader will in fact singnal an error
  ;; trying to do this
  (format stream "#<QT:[~a ~a ~a ~a]>"
	  ;; TODO: really keep the order w x y z ?
	  ;; now VECTORIZE shows different order then our print representation :I
	  (q.w q) (q.x q) (q.y q) (q.z q)))

(defun quaternion (w x y z)
  "Return a quaternion using the exact same input, no sin/cos deg->rad conversions,
see MAKE-QUAT for an intuitive constructor."
  (make-instance 'quat :w w :x x :y y :z z ))

(defgeneric vectorize (obj)
  (:documentation "Return a vector from object"))
(defmethod vectorize ((q quat))
  ;; TODO: is the order right? Note that GLM's quaternion is: w, x, y, z while in
  ;; the arc-book it is consistently: x, y, z, w hence that's what I'm using for now
  (glm:vec4 (q.x q) (q.y q) (q.z q) (q.w q)))


(defmacro make-quat (angle-deg (axis-x axis-y axis-z))
  "Providing an angle in degree and an axis: a quaternion is created and returned.
If the axis provided is of unit length the resulting quaternion will also be of
unit length (this is an intrinsic mathematical property of quaternions)."
  `(vec4->quat (glm:vec4 (float (framework:deg-to-rad ,angle-deg) 1.0)
			 (float ,axis-x 1.0)
			 (float ,axis-y 1.0)
			 (float ,axis-z 1.0)
                         )))


(defun vec4->quat (vec4)
  "Transform a 4D vector to get a quaternion. Input is treated as: 
 (theta axis-x axis-y axis-z). Where theta is treated as RADIAN. If 
the axis provided is already of unit length, the result is a _unit quaternion_."
  (let* ((theta (vec. vec4 :x))
	 (w (cos (/ theta 2)))
	 (x (* (vec. vec4 :y) (sin (/ theta 2))))
	 (y (* (vec. vec4 :z) (sin (/ theta 2))))
	 (z (* (vec. vec4 :w) (sin (/ theta 2)))))
    (quaternion w x y z)))

;; naming this just '*' as in providing operator overloading, is not possible
;; error will be singnaled: "'*' already names an ordinary function or a macro."
(defgeneric quat* (quat quat))
(defmethod quat* ((q1 quat) (q2 quat))
  "Quaternion multiplication"
  (let* ((a.w (q.w q1)) (a.x (q.x q1)) (a.y (q.y q1)) (a.z (q.z q1))
	 (b.w (q.w q2)) (b.x (q.x q2)) (b.y (q.y q2)) (b.z (q.z q2)))
    ;; Quaternion multiplication being a composition orientation the result is already
    ;; a quaternion. Hence we directly make an instance without MAKE-QUATERNION
    (make-instance
     'quat
     :x (- (+ (* a.w b.x) (* a.x b.w) (* a.y b.z)) (* a.z b.y))
     :y (- (+ (* a.w b.y) (* a.y b.w) (* a.z b.x)) (* a.x b.z))
     :z (- (+ (* a.w b.z) (* a.z b.w) (* a.x b.y)) (* a.y b.x))
     :w (-    (* a.w b.w) (* a.x b.x) (* a.y b.y)  (* a.z b.z)) )))



;; argh, lock on symbol "CONJUGATE". In quaternion lingo the inverse of a quaternion
;; called the "conjugate quaternion"
(defun conjugate-quat (quat)
  "Return the conjugate quaternion of the input quaternion."
  (quaternion (q.w quat)
	    (- (q.x quat))
	    (- (q.y quat))
	    (- (q.z quat))))



;;alas using a class to represent quaternions (useful as '*' can't be casted so we need
;;'quat*' for a quaternion multiplication procedure. Now because our quaternion is a class
;;we can't use NORMALIZE on it, and again need to create a new name for it: Anyway this
;;distinction may be favorable as quaternion normalization may be a bit different as it
;;only involves the normalization of the axis part of the quaternion (not the scalar)
;;TODO: confirm this
(defgeneric quat-normalize (quat))
(defmethod quat-normalize ((q quat))
  ;; from cprogramming.com By "confuted":
  ;; magnitude = sqrt(w2 + x2 + y2 + z2)
  ;; w = w / magnitude
  ;; x = x /  magnitude
  ;; y = y / magnitude
  ;; z = z / magnitude
  ;; which means quaternion normalization is quite simple
  ;; Note the normalization process doesn't seem to retain the
  ;; precise angle of rotation, therefore before each application the rotation angle
  ;; should be cached and after the normalization overwritten.
  (let ((q-v4 (vectorize q)))
    (multiple-value-bind (x y z w)
	(values-list
	 (loop for i across (normalize q-v4) collecting i))
      (make-instance 'quat :w w :x x :y y :z z))))



;; TODO: test this function, not safe to use yet:
;; This function is only for tests, rounding is not working the way it is expected
;; e.g. (round 1.5) => 2.0!
(defun round-matrix (mat4)
  (sb-cga:transpose-matrix
   (apply #'sb-cga:matrix
	  (loop for x across mat4
	     collecting (coerce (round x) 'single-float)))))

(defun m. (row col mat4)
  (aref mat4 (+ row (* 4 col))))

;; this is a straight translation from the GLM library, minus the templates
;; https://github.com/g-truc/glm/blob/master/glm/gtc/quaternion.inl
;; which is the same library used by the arcsynthesis code
(defun quat-cast (mat4)
  "Return the quaternion representing the input transformation matrix."
  ;; in the C++ glm code "template <typename T, precision P>" is used, which seems to
  ;; be a function overloading shorthand, wheras the function must be specified
  ;; once using those templates(...) as input/output and the compiler will create
  ;; all the functions overloading needed depending on function use: foo(int), foo(double)
  ;; would then work. For us this means we will ignore that and just focus on a mat4-float
  ;; implementation of quat-cast.
  ;;
  ;; note the switched col/row :I GLM seems to represent matrices differently or
  ;; transposes somewhere or.. whatever it works
  (flet ((m (col row) 
	   (aref mat4 (+ row (* 4 col)))))
    (let* ((four-x-squared-minus-1 (- (m 0 0) (m 1 1) (m 2 2)))
	   (four-y-squared-minus-1 (- (m 1 1) (m 0 0) (m 2 2)))
	   (four-z-squared-minus-1 (- (m 2 2) (m 0 0) (m 1 1)))
	   (four-w-squared-minus-1 (+ (m 0 0) (m 1 1) (m 2 2)))

	   (biggest-index 0)
	   (four-biggest-squared-minus-1 four-w-squared-minus-1))
      ;; oh wow: c++ if(statement) { if statement true;
      ;;                             every line within these;
      ;;                             will be executed because;}
      ;;                           else {
      ;;                             There is an explicit;
      ;;                             ELSE block!!!;};
      ;;  hence a single if(predicate) { code;}; is equal to cl's WHEN
      (when (> four-x-squared-minus-1 four-biggest-squared-minus-1)
	(setf four-biggest-squared-minus-1 four-x-squared-minus-1)
	(setf biggest-index 1))

      (when (> four-y-squared-minus-1 four-biggest-squared-minus-1)
	(setf four-biggest-squared-minus-1 four-y-squared-minus-1)
	(setf biggest-index 2))

      (when (> four-z-squared-minus-1 four-biggest-squared-minus-1)
	(setf four-biggest-squared-minus-1 four-z-squared-minus-1)
	(setf biggest-index 3))

      (let* ((biggest-val (* (sqrt (+ four-biggest-squared-minus-1 1.0))  0.5))
	     (mult (/ 0.25 biggest-val))
	     (result))			; quaternion to be returned
	(setf
	 result
	 (case biggest-index
	   (0
	    (quaternion biggest-val
			(* (- (m 1 2) (m 2 1)) mult)
			(* (- (m 2 0) (m 0 2)) mult)
			(* (- (m 0 1) (m 1 0)) mult)))
	   (1
	    (quaternion (* (- (m 1 2) (m 2 1)) mult)
			biggest-val
			(* (+ (m 0 1) (m 1 0)) mult)
			(* (+ (m 2 0) (m 0 2)) mult)))
	   (2
	    (quaternion (* (- (m 2 0) (m 0 2)) mult)
			(* (+ (m 0 1) (m 1 0)) mult)
			biggest-val
			(* (+ (m 1 2) (m 2 1)) mult)))
	   (3 
	    (quaternion (* (- (m 0 1) (m 1 0)) mult)
			(* (+ (m 2 0) (m 0 2)) mult)
			(* (+ (m 1 2) (m 2 1)) mult)
			biggest-val))
	   (t (error "'biggest-index' out of bounds"))))
	result))))


;;2nd try, using information from:
;; www.euclideanspace.com/maths/geometry/rotations/conversions/matrixToQuaternion/
;; Knock on wood!
;; This only words in cases where the diagonal of the matrix is positive note the
;; the sqrt computation
;; (defun quat-cast-2nd (mat4)
;;   (flet ((m (r c)
;; 	   (aref mat4 (+ r (* 4 c)))))
;;     (let* ((w  (/ (sqrt (+ 1.0 (m 0 0) ( m 1 1) (m 2 2))) 2.0))
;; 	   (w4 (* 4.0 w))
;; 	   (x (/ (- (m 2 1) (m 1 2)) w4))
;; 	   (y (/ (- (m 0 2) (m 2 0)) w4))
;; 	   (z (/ (- (m 1 0) (m 0 1)) w4)))
;;       (let ((q (make-instance 'quat :w w :x x :y y :z z)))
;; 	q))))

;; supposed to cast from multiple structures or types to a matrix, for now only
;; from quaternion
;; TODO: revisit this 'initiation' I think it was clock-wise in the former implementation
;; For intuitition: inspecting the (mat4-cast (make-quat 45.0 (0.0 1.0 0.0))), for example, shows
;; that quaternion perform a clock-wise rotation around the axis they represent (in direction of the axis)
(defgeneric mat4-cast (t))
(defmethod mat4-cast ((q1 quat))
  "Retruns the transformation matrix the input quaternion is representing"
  (let ((w (q.w q1))
	(x (q.x q1))
	(y (q.y q1))
	(z (q.z q1))
	(mat4 (glm:make-mat4 1.0)))
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
    mat4))

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
