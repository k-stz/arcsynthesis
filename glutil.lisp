(in-package #:glutil)

(defclass matrix-stack ()
  ((m-curr-mat :initform (glm:make-mat4 1.0)
	       :accessor m-curr-mat)
   ;; is supposed to be a simple stack, so it is just a list here
   (m-matrices :initform (list)
	       :accessor m-matrices)))

;;TODO: with-matrix-stack that deletes object at the end? From what I read,
;; garbage collection facilitation is bad style since it is there to alleviate
;; the programmer of this burden. How to even force object deletion? Probably
;; inefficient to force destruction of object or..??

(defgeneric set-matrix (matrix-stack mat4))
(defmethod set-matrix ((ms matrix-stack) (mat4 simple-array))
  "Set the current (top) matrix of the matrix-stack to the given mat4"
  (setf (m-curr-mat ms) mat4))

(defgeneric apply-matrix (matrix-stack mat4))
(defmethod apply-matrix ((ms matrix-stack) (mat4 simple-array))
  "Apply the matrix to the current matrix in the matrix-stack and make the
Result the new current matrix"
  (let ((curr (top-ms ms)))
    (setf (m-curr-mat ms)
	  (sb-cga:matrix* curr mat4))))

(defgeneric top-ms (matrix-stack))
(defmethod top-ms ((ms matrix-stack))
  "Returns the current-matrix"
  (m-curr-mat ms))

;; TODO: lock on "push" even though it is cl:push is a function not a generic method
(defgeneric push-ms (matrix-stack))
(defmethod push-ms ((ms matrix-stack))
  "PUSH the current-matrix on the internal stack"
  (push (m-curr-mat ms) (m-matrices ms)))

(defgeneric pop-ms (matrix-stack))
(defmethod pop-ms ((ms matrix-stack))
  "POP last PUSHed matrix and set it to the current-matrix"
  ;; (first (m-matrices ms)) = m-matrices.top() note: m-matrices is supposed to be a
  ;; simple stack!
  (setf (m-curr-mat ms) (first (m-matrices ms)))
  (pop (m-matrices ms)))


(defun calc-frustum-scale (f-fov-deg)
  "the field-of-view (fov) is the angle between the forward direction and the direction
of the farmost-extent of the view (meaning vectors from these points still get to hit
the projection plane)"
  (let* ((deg-to-rad (/ (* pi 2.0) 360.0))
	(f-fov-rad (* f-fov-deg deg-to-rad)))
    (coerce
     (/ 1.0
	(tan (/ f-fov-rad 2.0)))
     'single-float)))

(defgeneric perspective (matrix-stack deg-FOV aspect-ratio z-near z-far))
(defmethod perspective ((ms matrix-stack) (deg-FOV float) (aspect-ratio float)
			(z-near float) (z-far float))
  ;; TODO: implement "aspect-ratio" usage. arc: uses in reshape(int w, int h)
  ;; like so (w / (float)h)
  (let ((m (glm:make-mat4 0.0))
	(frustum-scale (calc-frustum-scale deg-FOV)))
    (glm:set-mat4 m 0 :x frustum-scale)
    (glm:set-mat4 m 1 :y frustum-scale)
    (glm:set-mat4 m 2 :z (/ (+ z-far z-near)
			    (- z-near z-far)))
    (glm:set-mat4 m 2 :w -1.0)
    (glm:set-mat4 m 3 :z (/ (* 2 z-far z-near)
			    (- z-near z-far)))
    (set-matrix ms m)))


;; apply arbitrary matrix to the top matrix, m-curr-mat.
;; TODO: how to write methods which should only take 'matrix' type '(SIMPLE-ARRAY SINGLE-FLOAT (16))
;; note that a DEFGENERIC seems to just be a name and a specific amount of parameters, where
;; the defmethod is the only one specifying a class:
;; ==> (defmethod foo ((var-name class) no-class) <body>)
(defgeneric apply-matrix (matrix-stack matrix))
;;       as it is now "transform-matrix" can take any symbole because it was not closer specified
(defmethod apply-matrix ((ms matrix-stack) transform-matrix)
  (setf (m-curr-mat ms)
   (sb-cga:matrix* (m-curr-mat ms) transform-matrix)))


;; TODO: defmethod lambda-list displayed with class-name instead of "offset-vec3"
;; it's "simple-array"
(defgeneric translate (matrix-stack simple-array))
(defmethod translate ((ms matrix-stack) (offset-vec3 simple-array))
  "Translate transform the current-matrix by given vec3"
  (let ((translate-mat4 (glm:make-mat4 1.0))
	(vec4 (glm:vec3->vec4 offset-vec3)))
    (glm:set-mat4-col translate-mat4 3 vec4)
    (setf (m-curr-mat ms) (sb-cga:matrix* translate-mat4
					  (m-curr-mat ms)))))


(defgeneric rotate-x (matrix-stack float))
(defmethod rotate-x ((ms matrix-stack) (ang-deg float))
  (let ((transform-mat4 (glm:rotate-x ang-deg)))
    (setf (m-curr-mat ms) (sb-cga:matrix* (m-curr-mat ms)
					  transform-mat4))))

(defgeneric rotate-y (matrix-stack float))
(defmethod rotate-y ((ms matrix-stack) (ang-deg float))
  (let ((transform-mat4 (glm:rotate-y ang-deg)))
    (setf (m-curr-mat ms) (sb-cga:matrix* (m-curr-mat ms)
					  transform-mat4))))

(defgeneric rotate-z (matrix-stack float))
(defmethod rotate-z ((ms matrix-stack) (ang-deg float))
  (let ((transform-mat4 (glm:rotate-z ang-deg)))
    (setf (m-curr-mat ms) (sb-cga:matrix* (m-curr-mat ms)
					  transform-mat4))))

(defgeneric scale (matrix-stack simple-array))
(defmethod scale ((ms matrix-stack) (scale-vec simple-array))
  (let ((scale-mat4 (glm:make-mat4 1.0)))
    (glm:set-mat4-diagonal scale-mat4
  			   (glm:vec3->vec4 scale-vec))
    (setf (m-curr-mat ms) (sb-cga:matrix* (m-curr-mat ms)
					  scale-mat4))))

(defun draw-matrix-stack (matrix-stack mat-unif index-data)
      (gl:uniform-matrix mat-unif 4
			 (vector (top-ms matrix-stack)) NIL)
      (%gl:draw-elements :triangles (gl::gl-array-size index-data)
			 :unsigned-short 0))


;; (&key (drop t)) removed as having to provide args (matrix-stack-top-to-shader ..) is too
;; specific to abstract away in the macro
(defmacro with-transform ((matrix-stack) &body body)
    "Creates PUSH-MS POP-MS wrapper around its input, so many with-transform can be nested
to facilitate the hierarchical model. Intuitive explanation: work with the current
matrix (translating, scaling, rotating or arbitrary applying a transform matrix) and when your done, 
it will be returned to its former state"
    ;;TODO: add searching &body for transform keywords, so as to implement nested
    ;; constructs like:
    ;; (with-transform (ms) (cons :this (:that :rotate-x 10.0) (:this :scale ..)))
  (labels ((try-key (car l exp)
	     (cond ((null l)
		    (nreverse exp))
		   ;; this ensures that the body may contain arbitrary functions,
		   ;; and only free keywords get special treatment
		   ((listp car) (progn (push car exp)
				       (try-key (cadr l) (cdr l) exp)))
		   ;; keyword special treatment
		   (t (case car
			(:translate (make-trans 'translate (cdr l) exp))
			(:scale (make-trans 'scale (cdr l) exp))
			(:rotate-x (make-rot 'rotate-x (cdr l) exp))
			(:rotate-y (make-rot 'rotate-y (cdr l) exp))
			(:rotate-z (make-rot 'rotate-z (cdr l) exp))
			(:apply-matrix (make-apply-matrix 'apply-matrix (cdr l) exp))
			(t (format *error-output* "~a is not a known transform" car))))))
	   ;; special treatment code
	   (make-trans (key l exp)
	     (push
	      `(,key ,matrix-stack (glm:vec3 ,(first l) ,(second l) ,(third l)))
	      exp)
	     (try-key (cadddr l) (cdddr l) exp))
	   (make-rot (key l exp)
	     (push
	      `(,key ,matrix-stack ,(first l))
	      exp)
	     (try-key (cadr l) (cdr l) exp))
	   ;; like make-rot we only read one sexp after the keyword - it wants a mat4
	   ;; instead of an angle
	   (make-apply-matrix (key l exp)
	     (push
	      `(,key ,matrix-stack ,(first l))
	      exp)
	     (try-key (cadr l) (cdr l) exp)))
    ;; start labels body:
    `(progn
       (push-ms ,matrix-stack)
       ,@(try-key (car body) body '())
       (pop-ms ,matrix-stack))))


;;------------------------------------------------------------------------------
;;ViewPole implementation:
;; glutil::ViewPole g_viewPole = glutil::ViewPole(g_initialViewData,
;; 					       g_viewScale,
;;                                                glutil::MB_LEFT_BTN) ;

;; Implementation following the specification in "chapter 9 - Lights On"
(defclass view-pole ()
  ((curr-quat :initform (glm:quaternion 1.0 0.0 0.0 0.0) :accessor quat)
   (look-pt :initform (glm:vec3 0.0 0.0 0.0) :initarg :look-pt :accessor look-pt)
   (cam-pos :initform (glm:vec3 0.0 0.0 1.0) :initarg :cam-pos :accessor cam-pos)
   (up-pt :initform (glm:vec3 0.0 1.0 0.0) :initarg up-pt :accessor up-pt)
   (look-dir :accessor look-dir)
   (look-at-matrix :accessor look-at-mat4)))

(defmethod initialize-instance :after ((vp view-pole) &key)
  ;; calculate look-direction of view-pole
  (let ((cam (cam-pos vp))
	(look-pt (look-pt vp)))
    (setf (look-dir vp) (sb-cga:normalize
			 (glm:vec- look-pt cam)))))

(defun update-look-dir (view-pole)
  (let ((mat (glm:mat4-cast (quat view-pole))))
    (setf (look-dir view-pole)
	  (glm:vec4->vec3
	   (glm:mat*vec mat (glm:vec3->vec4 (look-dir view-pole)))))))

;; TODO: abstract
(defun rotate-vp-y (deg view-pole)
  (let* ((trans-quat (glm:make-quat deg (0.0 1.0 0.0)))
	 (vp-quat (quat view-pole))
	 (result (glm:quat* vp-quat trans-quat)))
    (setf (quat view-pole) result)))

(defun rotate-vp-x (deg view-pole)
  (let* ((trans-quat (glm:make-quat deg (1.0 0.0 0.0)))
	 (vp-quat (quat view-pole))
	 (result (glm:quat* vp-quat trans-quat)))
    (setf (quat view-pole) result)))

(defun rotate-vp-z (deg view-pole)
  (let* ((trans-quat (glm:make-quat deg (0.0 0.0 1.0)))
	 (vp-quat (quat view-pole))
	 (result (glm:quat* vp-quat trans-quat)))
    (setf (quat view-pole) result)))


;; TODO: clean up
(defun move-camera (view-pole vec3-direction)
  (let ((pos (cam-pos view-pole))
	(vp-mat (glm:mat4-cast (quat view-pole))))
    (setf (cam-pos view-pole)
    	  (sb-cga:vec+ (cam-pos view-pole) (sb-cga:normalize vec3-direction)))
    ;; (setf new-pos
    ;; 	  (sb-cga:vec+ pos (sb-cga:normalize vec3-direction)))
    ;; (glm:vec4->vec3
    ;;  (glm:mat*vec vp-mat
    ;; 		  (glm:vec3->vec4 new-pos)))
    ))


;; NEXT-TODO: always have the old representation, and just add the new `view-pole*pos
;;            well and the new position is the one used for orientation. Hence You always
;;            need to store a transformation for the foundational matrix????

(defgeneric calc-matrix (pole-object))
(defmethod calc-matrix ((vp view-pole))
  (let ((mat (glm:mat4-cast (quat vp)))
	(cam-pos-mat (sb-cga:translate (glm:vec- (cam-pos vp)))))
    ;;updating look-dir TODO: make this more central somewhere more upstream?
;    (update-look-dir vp)
    ;; Reversing the order here allows for camera-relative, or model-relative
    ;; transformation!
    (sb-cga:matrix* mat cam-pos-mat)))

;; (defun calc-matrix (view-pole)
;;   (let ((look-at-matrix
;; 	 (calc-look-at-matrix (slot-value view-pole 'cam-pos)
;; 			      (slot-value view-pole 'look-pt)
;; 			      (slot-value view-pole 'up-pt))))
;;     (setf (look-at-mat4 view-pole) look-at-matrix)
;;     look-at-matrix))

(defun calc-look-at-matrix (camera-pt look-pt up-pt)
  "Returns a transformation matrix that represents an orientation of a camera orientation
described by the arguments given."
  ;; TODO: migrate explanation
  ;; explanation in "world-with-ubo.lisp" in the chapter 7 directory
  (let* ((look-dir (sb-cga:normalize (sb-cga:vec- look-pt camera-pt)))
	 (up-dir (sb-cga:normalize up-pt))
	 (right-dir (sb-cga:normalize (sb-cga:cross-product look-dir up-dir)))
	 (perp-up-dir (sb-cga:cross-product right-dir look-dir))

	 (rot-mat (glm:make-mat4 1.0))
	 (trans-mat (glm:make-mat4 1.0)))

    (glm:set-mat4-col rot-mat 0 (glm:vec3->vec4 right-dir 0.0))
    (glm:set-mat4-col rot-mat 1 (glm:vec3->vec4 perp-up-dir 0.0))
    (glm:set-mat4-col rot-mat 2 (glm:vec3->vec4 (glm:vec- look-dir) 0.0))
    ;; TODO: why transpose it eventually? Maybe because it is col-major and setting
    ;; column-wise and transpose is more efficient than just setting the rows
    ;; with discontiguous indices
    (setf rot-mat (sb-cga:transpose-matrix rot-mat))
    (glm:set-mat4-col trans-mat 3 (glm:vec3->vec4 (glm:vec- camera-pt) 1.0))
    (sb-cga:matrix* rot-mat trans-mat)))

;; Object-Pole bare minimal implementation:
(defclass object-pole ()
  ((pos :initform (glm:vec3 0.0 0.0 0.0) :initarg :pos :accessor pos)
   (orientation :initform (glm:quaternion 1.0 0.0 0.0 0.0)
		:initarg :orient
		:accessor orient)))


(defmethod calc-matrix ((obj-p object-pole))
  (let ((translate-mat (glm:make-mat4 1.0)))
    (glm:set-mat4-col translate-mat 3
		      (glm:vec3->vec4 (pos obj-p)))
    (sb-cga:matrix* translate-mat (glm:mat4-cast (orient obj-p)))))

;;------------------------------------------------------------------------------

(defun round-matrix (mat)
  ;; TODO: cut of decimal places to make it look symmetrical for ease of reading
  (loop for el across mat
     for i = 0 then (1+ i)
     :do
       (when (and (< el 0.00001)
		  (> el -0.00001))
	 (setf (aref mat i) 0.0)))
  mat)

