;; TODO: about

(in-package #:arc-6.3)

(defvar *glsl-directory*
  (merge-pathnames #p "6-chapter/" (asdf/system:system-source-directory :arcsynthesis)))
;;todo: fix this output to slime-repl solution
(defvar out *standard-output*)  (defvar dbg *debug-io*) (defvar err *error-output*)

(defvar position-buffer-object) ; buffer object handle


(defvar *program*)

(defparameter *camera-to-clip-matrix* (glm:make-mat4 0.0))
(defvar *camera-to-clip-matrix-unif*)

(defvar *model-to-camera-matrix-unif*)

(defvar *scale-clip-matrix-unif*)


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

;; TODO: why does it look smaller than the screenshots?
;; provisional solution to scale problem using 25.0
(defparameter *frustum-scale* (calc-frustum-scale 25.0)) 

(defun initialize-program ()
  (let ((shader-list (list)))
    ;;oh c'mon how to make it local
    (push (arc:create-shader
	   :vertex-shader
	   (arc:file-to-string
	    (merge-pathnames "pos-color-local-transformation.vert" *glsl-directory*)))
	  shader-list)
    (push (arc:create-shader
    	   :fragment-shader
    	   (arc:file-to-string
	    (merge-pathnames "color-passthrough.frag" *glsl-directory* )))
    	  shader-list)
    (setf *program* (arc:create-program-and-return-it shader-list))

    (setf *model-to-camera-matrix-unif*
	  (gl:get-uniform-location *program* "model_to_camera_matrix"))
    (setf *camera-to-clip-matrix-unif*
	  (gl:get-uniform-location *program* "camera_to_clip_matrix"))

    (format t "a:~a b:~a" *model-to-camera-matrix-unif* *camera-to-clip-matrix-unif*)

    (let ((fz-near 1.0)
	  (fz-far 45.0))
      (glm:set-mat4 *camera-to-clip-matrix* 0 :x *frustum-scale*)
      (glm:set-mat4 *camera-to-clip-matrix* 1 :y *frustum-scale*)
      (glm:set-mat4 *camera-to-clip-matrix* 2 :z (/ (+ fz-far fz-near)
						    (- fz-near fz-far)))
      (glm:set-mat4 *camera-to-clip-matrix* 2 :w -1.0)
      (glm:set-mat4 *camera-to-clip-matrix* 3 :z (/ (* 2 fz-far fz-near)
						    (- fz-near fz-far)))
      (%gl:use-program *program*)
      
      (gl:uniform-matrix *camera-to-clip-matrix-unif*  4 (vector *camera-to-clip-matrix*)
			 :false))
    (%gl:use-program 0)
    (loop for shader-object in shader-list
       do (%gl:delete-shader shader-object))))


(defparameter *number-of-vertices* (* 6 4))

(defparameter +red-color+   '(1.0 0.0 0.0 1.0))
(defparameter +green-color+ '(0.0 1.0 0.0 1.0))
(defparameter +blue-color+  '(0.0 0.0 1.0 1.0))

(defparameter +yellow-color+ '(1.0 1.0 0.0 1.0))
(defparameter +cyan-color+ '(0.0 1.0 1.0 1.0))
(defparameter +magenta-color+ '(1.0 0.0 1.0 1.0))


(defparameter *vertex-data*
  (arc:create-gl-array-from-vector 
`#(
	;;Front
	+1.0  +1.0  +1.0 
	+1.0  -1.0  +1.0 
	-1.0  -1.0  +1.0 
	-1.0  +1.0  +1.0 

	;;Top
	+1.0  +1.0  +1.0 
	-1.0  +1.0  +1.0 
	-1.0  +1.0  -1.0 
	+1.0  +1.0  -1.0 

	;;Left
	+1.0  +1.0  +1.0 
	+1.0  +1.0  -1.0 
	+1.0  -1.0  -1.0 
	+1.0  -1.0  +1.0 

	;;Back
	+1.0  +1.0  -1.0 
	-1.0  +1.0  -1.0 
	-1.0  -1.0  -1.0 
	+1.0  -1.0  -1.0 

	;;Bottom
	+1.0  -1.0  +1.0 
	+1.0  -1.0  -1.0 
	-1.0  -1.0  -1.0 
	-1.0  -1.0  +1.0 

	;;Right
	-1.0  +1.0  +1.0 
	-1.0  -1.0  +1.0 
	-1.0  -1.0  -1.0 
	-1.0  +1.0  -1.0 


	,@+green-color+
	,@+green-color+
	,@+green-color+
	,@+green-color+

	,@+blue-color+
	,@+blue-color+
	,@+blue-color+
	,@+blue-color+

	,@+red-color+
	,@+red-color+
	,@+red-color+
	,@+red-color+

	,@+yellow-color+
	,@+yellow-color+
	,@+yellow-color+
	,@+yellow-color+

	,@+cyan-color+
	,@+cyan-color+
	,@+cyan-color+
	,@+cyan-color+

	,@+magenta-color+
	,@+magenta-color+
	,@+magenta-color+
	,@+magenta-color+
  )))

(defparameter *index-data*
  (arc::create-gl-array-of-unsigned-short-from-vector
   #(
	0  1  2 
	2  3  0 

	4  5  6 
	6  7  4 

	8  9  10 
	10  11  8 

	12  13  14 
	14  15  12 

	16  17  18 
	18  19  16 

	20  21  22 
	22  23  20 
     )))

(defvar *vertex-buffer-object*)
(defvar *index-buffer-object*)

(defun initialize-vertex-buffer ()
  (setf *vertex-buffer-object* (first (gl:gen-buffers 1)))

  (gl:bind-buffer :array-buffer *vertex-buffer-object*)
  (gl:buffer-data :array-buffer :static-draw *vertex-data*)
  (gl:bind-buffer :array-buffer 0)

  ;; index-array time:
  (setf *index-buffer-object* (first (gl:gen-buffers 1)))

  (gl:bind-buffer :element-array-buffer  *index-buffer-object*)
  (gl:buffer-data :element-array-buffer  :static-draw *index-data*)
  (gl:bind-buffer :element-array-buffer  0)  
  )

(defvar *vao*)

(defun initialize-vertex-array-objects ()
  (setf *vao* (first (gl:gen-vertex-arrays 1)))
  (gl:bind-vertex-array *vao*)

  (let ((color-data-offset (* #|size-of(float):|# 4 3 *number-of-vertices*)))
    (gl:bind-buffer :array-buffer *vertex-buffer-object*)
    (%gl:enable-vertex-attrib-array 0)
    (%gl:enable-vertex-attrib-array 1)
    (%gl:vertex-attrib-pointer 0 3 :float :false 0 0)
    (%gl:vertex-attrib-pointer 1 4 :float :false 0 color-data-offset)
    (%gl:bind-buffer :element-array-buffer *index-buffer-object*)

    (%gl:bind-vertex-array 0)
    )
  )




(defun init ()
	(initialize-program)
	(initialize-vertex-buffer)
	(initialize-vertex-array-objects)
  
	(gl:enable :cull-face)
	(%gl:cull-face :back)
	(%gl:front-face :cw)

	(gl:viewport 0 0 500 500)

	(gl:enable :depth-test)
	(gl:depth-mask :true)
	(%gl:depth-func :lequal)
	(gl:depth-range 0.0 1.0)
)


(defun translate-matrix-from-vec3 (vec3)
  (let ((matrix (glm:make-mat4 1.0)))
    (glm:set-mat4-col matrix 3
		      (glm:vec4-from-vec3 vec3))
    matrix))

(defun scale-matrix-from-vec3 (vec3)
  (let ((matrix (glm:make-mat4 1.0)))
    (glm:set-mat4-diagonal matrix (glm:vec4-from-vec3 vec3))
    matrix))



;;TODO: implement matrix-stack methods, change and fully implemet (draw) and (display) code
;; TODO: creating clas overkill? Simpler solution?
(defclass matrix-stack ()
  ;; Hierarchy.cpp really uses these to as "private" 
  ((m-curr-mat :initform (glm:make-mat4 1.0)
	       :accessor m-curr-mat)
   ;; is supposed to be a simple stack, so it is just a list here
   (m-matrices :initform (list)
	       :accessor m-matrices)))

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

;; TODO: defmethod lambda-list displayed with class-name instead of "offset-vec3"
;; its "simple-array"
(defgeneric translate (matrix-stack simple-array))
(defmethod translate ((ms matrix-stack) (offset-vec3 simple-array))
  "Trasnlate transform the current-matrix by given vec3"
  (let ((translate-mat4 (glm:make-mat4 1.0))
	(vec4 (glm:vec4-from-vec3 offset-vec3)))
    (glm:set-mat4-col translate-mat4 3 vec4)
    (setf (m-curr-mat ms) (sb-cga:matrix* translate-mat4
					  (m-curr-mat ms)))))


(defgeneric rotate-x (matrix-stack float))
(defmethod rotate-x ((ms matrix-stack) (ang-deg float))
  (let ((translate-mat4 (glm:rotate-x ang-deg)))
    (setf (m-curr-mat ms) (sb-cga:matrix* translate-mat4
					  (m-curr-mat ms)))))
;;TODO: test
(defgeneric rotate-y (matrix-stack float))
(defmethod rotate-y ((ms matrix-stack) (ang-deg float))
  (let ((translate-mat4 (glm:rotate-y ang-deg)))
    (setf (m-curr-mat ms) (sb-cga:matrix* translate-mat4
					  (m-curr-mat ms)))))

(defgeneric rotate-z (matrix-stack float))
(defmethod rotate-z ((ms matrix-stack) (ang-deg float))
  (let ((translate-mat4 (glm:rotate-x ang-deg)))
    (setf (m-curr-mat ms) (sb-cga:matrix* translate-mat4
					  (m-curr-mat ms)))))


(defgeneric scale (matrix-stack simple-array))
(defmethod scale ((ms matrix-stack) (scale-vec simple-array))
  (let ((scale-mat4 (glm:make-mat4 1.0)))
    (glm:set-mat4-diagonal scale-mat4
  			   (glm:vec4-from-vec3 scale-vec))
    (setf (m-curr-mat ms) (sb-cga:matrix* scale-mat4
					  (m-curr-mat ms)))))


;(defparameter *model-to-camera-stack* (make-instance 'matrix-stack))
(defvar *model-to-camera-stack*)

(defun matrix-stack-top-to-shader-and-draw (matrix-stack)
      (gl:uniform-matrix *model-to-camera-matrix-unif* 4
			 (vector (top-ms matrix-stack)))
      (%gl:draw-elements :triangles (gl::gl-array-size *index-data*)
			 :unsigned-short 0))

;;TODO: actually using :drawp nil, causes the code to expand the WHEN to a NIL
;;      try to remove that
(defmacro with-transform ((&key (drawp t)) matrix-stack &body body)
  "Creates PUSH-MS POP-MS wrapper around its input, so many with-transform can
be nested to facilitate the hierarchical model."
  `(progn
     (push-ms ,matrix-stack)
     ,@body ;; put another with-transform here
     ,(when drawp
	    `(matrix-stack-top-to-shader-and-draw ,matrix-stack))
     (pop-ms ,matrix-stack)))

(defparameter *for-w-s-key* 0.0)
(defparameter *for-a-d-key* 0.0)
(defparameter *for-q-e-key* 0.0)

(defparameter *pos-base* (glm:vec3 3.0 -5.0 -40.0))
(defparameter *ang-base* 45.0) 
(defparameter *pos-base-left* (glm:vec3 2.0 0.0 0.0))
(defparameter *pos-base-right* (glm:vec3 -2.0 0.0 0.0))
(defparameter *scale-base-z* 3.0)
(defparameter *ang-upper-arm* 33.75)
(defparameter *size-upper-arm* 9.0)
(defparameter *pos-lower-arm* (glm:vec3 0.0 0.0 8.0))
(defparameter *ang-lower-arm* -146.25)
(defparameter *len-lower-arm* 5.0)
(defparameter *width-lower-arm* 1.5)
(defparameter *pos-wrist* (glm:vec3 0.0 0.0 5.0))
(defparameter *ang-wrist-roll* 0.0)
(defparameter *ang-wrist-pitch* -67.5)
(defparameter *len-wrist* 2.0)
(defparameter *width-wrist* 2.0)
(defparameter *pos-left-finger* (glm:vec3 1.0 0.0 1.0))
(defparameter *pos-right-finger* (glm:vec3 -1.0 0.0 1.0))
(defparameter *ang-finger-open* -180.0)
(defparameter *len-finger* 2.0)
(defparameter *width-finger* 0.5)
(defparameter *ang-lower-finger* -45.0)

(defun draw ()
  ;;OOOOH we really need to "create" a new stack on every iteration, don't
  ;;we create a lot of "garbage" this way?
  (setf *model-to-camera-stack* (make-instance 'matrix-stack))

  ;;TODO CONTINUE: implement arcsynthesis armature
  (translate *model-to-camera-stack* *pos-base*)
  (rotate-y *model-to-camera-stack* *ang-base*)

  ;; The lesson here is that SCALE is a distorting function and transform containing
  ;; it should bear no children! While, TRANSLATE and ROTATE-* are additive in and
  ;; hence intuitive in their behaviour

  ;; Draw left base
  (with-transform () *model-to-camera-stack*
    (translate *model-to-camera-stack* *pos-base-left*)
    (scale *model-to-camera-stack* (glm:vec3 1.0 1.0 *scale-base-z*)))
  ;; Draw right base
  (with-transform () *model-to-camera-stack*
    (translate *model-to-camera-stack* *pos-base-right*)
    (scale *model-to-camera-stack* (glm:vec3 1.0 1.0 *scale-base-z*)))

  ;; Draw main arm; DrawUpperArm(modelToCameraStack);
  (with-transform () *model-to-camera-stack*
    (rotate-x *model-to-camera-stack* *ang-upper-arm*)
    (with-transform () *model-to-camera-stack*
      (translate *model-to-camera-stack*
		 (glm:vec3 0.0 0.0 (- (/ *size-upper-arm* 2.0) 1.0)))
      (scale *model-to-camera-stack* (glm:vec3 1.0 1.0 (/ *size-upper-arm* 2.0))))

    ;; DrawLowerArm(...)
    (with-transform () *model-to-camera-stack*
      (translate *model-to-camera-stack* *pos-lower-arm*)
      (rotate-x *model-to-camera-stack* *ang-lower-arm* )

      (with-transform () *model-to-camera-stack*
	(translate *model-to-camera-stack* (glm:vec3 0.0 0.0 (/ *len-lower-arm* 2.0)))
	(scale *model-to-camera-stack* (glm:vec3 (/ *width-lower-arm* 2.0)
						 (/ *width-lower-arm* 2.0)
						 (/ *len-lower-arm* 2.0))))
      ;; DrawWrist(...)
      (with-transform (:drawp nil) *model-to-camera-stack*
	  (translate *model-to-camera-stack* *pos-wrist*)
	  (rotate-z *model-to-camera-stack* *ang-wrist-roll*)
	  (rotate-x *model-to-camera-stack* *ang-wrist-pitch*)

	  (with-transform () *model-to-camera-stack*
	    (scale *model-to-camera-stack* (glm:vec3 (/ *width-wrist* 2.0)
	  					     (/ *width-wrist* 2.0)
	  					     (/ *len-wrist* 2.0))))
	  ;; DrawFingers(...)
	  (with-transform (:drawp nil) *model-to-camera-stack*
	    ;; Draw left finger
	    (translate *model-to-camera-stack* *pos-left-finger*)
	    (rotate-y *model-to-camera-stack* *ang-finger-open*)

	    (with-transform () *model-to-camera-stack*
	      (translate *model-to-camera-stack* (glm:vec3 0.0 0.0 (/ *len-finger* 2.0)))
	      (scale *model-to-camera-stack* (glm:vec3 (/ *width-finger* 2.0)
						       (/ *width-finger* 2.0)
						       (/ *len-finger* 2.0))))
	    ;;Draw left lower finger
	    (with-transform (:drawp nil) *model-to-camera-stack*
	      (translate *model-to-camera-stack* (glm:vec3 0.0 0.0 *len-finger*))
	      (rotate-y *model-to-camera-stack* (- *ang-lower-finger*))
	      (with-transform () *model-to-camera-stack*
		(translate *model-to-camera-stack* (glm:vec3 0.0 0.0
							     (/ *len-finger* 2.0)))
		(scale *model-to-camera-stack* (glm:vec3 (/ *width-finger* 2.0)
							 (/ *width-finger* 2.0)
							 (/ *len-finger* 2.0))))
	      );;/lower left finger
	    )
	  ;;Draw right finger
	  ;; TODO CONTINUE + REVERSE ANGLES FIX
	  );;/DrawWrist
      ) ;;/DrawLowerArm
    ) ;;/Draw main arm
  
  )

(defun display ()
  (gl:clear-color 0 0 0.2 1)
  (gl:clear-depth 1.0)
  (gl:clear :color-buffer-bit :depth-buffer-bit)

  (%gl:use-program *program*)
  (gl:bind-vertex-array *vao*)

  (draw)
  (gl:bind-vertex-array 0)
  (gl:use-program *program*)
  ;;swap buffers: in main loop 
       )

;; teehee, just found a #define substitute
;; TODO: do symbol-macro somehow still clutter up space at runtime or after compilation?
(define-symbol-macro standard-angle-increment 11.25)
(define-symbol-macro small-angle-increment 9.0)

(defun clamp (x lower-bound upper-bound)
  (cond ((< x lower-bound)
	 lower-bound)
	((> x upper-bound)
	 upper-bound)
	(t ;;else
	 x)))

;; TODO: test
(defmacro adj-* (var inc-by bound-fn)
  `(progn (incf ,var ,inc-by)
	  ( setf ,var ,bound-fn)))

(defun main ()
  (sdl2:with-init (:everything)
    (progn (setf *standard-output* out) (setf *debug-io* dbg) (setf *error-output* err))
    (sdl2:with-window (win :w 500 :h 500 :flags '(:shown :opengl :resizable))
      (sdl2:with-gl-context (gl-context win)
	;; INIT code:
	(init)
	(sdl2:with-event-loop (:method :poll)
	  (:keydown
	   (:keysym keysym)
	   ;; TODO: capture in macro
	   (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-a)
	     (incf *ang-base* standard-angle-increment)
	     ;; ensures *ang-base* always stays withing [0,360]
	     (setf *ang-base* (mod *ang-base* 360.0)))
	   (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-d)
	     (decf *ang-base* standard-angle-increment)
	     (setf *ang-base* (mod *ang-base* 360.0)))

	   (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-w)
	     (incf *ang-upper-arm* standard-angle-increment)
	     (setf *ang-upper-arm* (clamp *ang-upper-arm* 0.0 90.0)))
	   (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-s)
	     (decf *ang-upper-arm* standard-angle-increment)
	     (setf *ang-upper-arm* (clamp *ang-upper-arm* 0.0 90.0)))

	   (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-r)
	     (incf *ang-lower-arm* standard-angle-increment)
	     (setf *ang-lower-arm* (clamp *ang-lower-arm* -146.25 0.0)))
	   (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-f)
	     (decf *ang-lower-arm* standard-angle-increment)
	     (setf *ang-lower-arm* (clamp *ang-lower-arm* -146.25 0.0)))


	   (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-q)
	     (decf *for-q-e-key* 10.0))
	   (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-e)
	     ;; experimental code
	     (incf *for-q-e-key* 10.0)
	     )
	   (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
	     (sdl2:push-event :quit)))
	  (:quit () t)
	  (:idle ()
		 ;;main-loop:
		 (display)
		 
                 (sdl2:gl-swap-window win) ; wow, this can be forgotten easily -.-
		 ))))))

