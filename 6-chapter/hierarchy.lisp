;; We need to create stack containing 3 matrices:
;; 1. scale-matrix already containing all its predecessors scale-matrix
;; 2. rotation 3. translation.
;;; Or does this waste too much space? 

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

;;all of these visible parameters have their classes as names?
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
			 (vector (top matrix-stack)))
      (%gl:draw-elements :triangles (gl::gl-array-size *index-data*)
			 :unsigned-short 0))

(defmacro with-transform (matrix-stack &body body)
  "Creates PUSH-MS POP-MS wrapper around its input, so many with-transform can
be nested to facilitate the hierarchical model."
  `(progn
     (push-ms ,matrix-stack)
     ,@body ;; put another with-transform here
     (pop-ms ,matrix-stack)))


(defparameter ang-base -45.0)
(defparameter *for-w-s-key* 0.0)
(defparameter *for-a-d-key* 0.0)
(defparameter *for-q-e-key* 0.0)

(defun draw ()
  (let ((pos-base (glm:vec3 3.0 -5.0 -40.0))
	;;TODO it seems to look on the picture like 45.0 is right, maybe
	;;rotate-y implementation is wrong?
;  	(ang-base -45.0)
;	(ang-base -90.0)
  	(scale-base-z 3.0)
  	(pos-base-left (glm:vec3 2.0 0.0 0.0))
  	(pos-base-right (glm:vec3 -2.0 0.0 0.0))
  	)
    (list pos-base ang-base scale-base-z pos-base-left pos-base-right)

    ;;OOOOH we really need to "create" a new stack on every iteration, don't
    ;;we create a lot of "garbage" this way?
    (setf *model-to-camera-stack* (make-instance 'matrix-stack))

    ;;TODO CONTINUE: implement arcsynthesis armature
    (translate *model-to-camera-stack* pos-base)
    (rotate-y *model-to-camera-stack* *for-q-e-key*)

    ;; Draw left base
    (with-transform *model-to-camera-stack*
      (translate *model-to-camera-stack* pos-base-left)
      (scale *model-to-camera-stack* (glm:vec3 1.0 1.0 scale-base-z))
      (matrix-stack-top-to-shader-and-draw *model-to-camera-stack*))
      (with-transform *model-to-camera-stack*
	(translate *model-to-camera-stack* (glm:vec3 0.0 4.0 0.0))
	(scale *model-to-camera-stack* (glm:vec3 1.0 4.0 1.0))
	(rotate-z *model-to-camera-stack* 90.0)
	(matrix-stack-top-to-shader-and-draw *model-to-camera-stack*)
	(with-transform *model-to-camera-stack*
	  (translate *model-to-camera-stack* (glm:vec3 -2.0 0.0 1.0))
	  (rotate-x *model-to-camera-stack* *for-a-d-key*)
	  (rotate-y *model-to-camera-stack* *for-w-s-key*)
	  (matrix-stack-top-to-shader-and-draw *model-to-camera-stack*)
	  ))
    ))

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
   	   (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-w)
	     (incf *for-w-s-key* 10.0))
	   (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-s)
	     (decf *for-w-s-key* 10.0))
	   
	   (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-a)
	     (incf *for-a-d-key* 10.0))
	   (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-d)
	     (decf *for-a-d-key* 10.0))

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

