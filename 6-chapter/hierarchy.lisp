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


(defun translate (vec3)
  (let ((tm (translate-matrix-from-vec3 vec3)))))


;;TODO: implement matrix-stack methods, change and fully implemet (draw) and (display) code
(defclass matrix-stack ()
  ((m-curr-mat :initform (glm:make-mat4 1.0))))

(defparameter *model-to-camera-stack* (make-instance 'matrix-stack))

(defun draw ()
  (let ((pos-base (glm:vec3 3.0 -5.0 -40.0))
	(ang-base -45.0)
	(scale-base-z 3.0)
	(pos-base-left (glm:vec3 2.0 0.0 0.0))
	(pos-base-right (glm:vec3 -2.0 0.0 0.0))
	)

    (setf (first *model-to-camera-stack*) (translate-matrix-from-vec3 pos-base))
    (setf (first *model-to-camera-stack*) (glm:rotate-y ang-base))


    (setf *armature-list*
	  ;; translate-matrix rotation-matrix scale-matrix
	  (list
	   ;; Left base
	   (list :translate (translate-matrix-from-vec3 pos-base)
		 :rotation (rotate-y ang-base))
	   ;; right base
	   (list :translate (translate-matrix-from-vec3 pos-base-left)
		 :scale (scale-matrix-from-vec3 (glm:vec3 1.0 1.0 scale-base-z)))
	   )
	  )))

(defun display ()
  (gl:clear-color 0 0 0.2 1)
  (gl:clear-depth 1.0)
  (gl:clear :color-buffer-bit :depth-buffer-bit)

  (%gl:use-program *program*)
  (gl:bind-vertex-array *vao*)

  (init-armture-list)
  
  (loop for matrix in *armature-list*
     with id = (glm:make-mat4 1.0) 
     for tm = (or (getf matrix :translate) id)
     for rm = (or (getf matrix :rotation) id) ;TODO
     for sm = (or (getf matrix :scale) id)
  
     ;; strangly sb-cga:matrix* seems to multiply right to left
     for transform-matrix = (sb-cga:matrix* sm rm tm)
     do
       (gl:uniform-matrix

	*model-to-camera-matrix-unif* 4 (vector transform-matrix))
       (%gl:draw-elements
	:triangles (gl::gl-array-size *index-data*) :unsigned-short 0))
       
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
	  (:keyup
	   (:keysym keysym)
	   (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-e)
	     ;; experimental code
	     )
	   (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
	     (sdl2:push-event :quit)))
	  (:quit () t)
	  (:idle ()
		 ;;main-loop:
		 (display)
		 
                 (sdl2:gl-swap-window win) ; wow, this can be forgotten easily -.-
		 ))))))

