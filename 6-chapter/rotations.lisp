(in-package #:arc-6.2)

(defvar *data-directory*
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

;; DONE: why does it look smaller than the screenshots?
;; provisional solution to scale problem using 25.0
;; => cam-matrix was wrong ALLLL ALONG, (essentially: row/col-major issue, just transposing
;;    it solved it!)
(defparameter *frustum-scale* (calc-frustum-scale 45.0)) 

(defun initialize-program ()
  (let ((shader-list (list)))
    ;;oh c'mon how to make it local
    (push (arc:create-shader
	   :vertex-shader
	   (arc:file-to-string
	    (merge-pathnames "pos-color-local-transformation.vert" *data-directory*)))
	  shader-list)
    (push (arc:create-shader
    	   :fragment-shader
    	   (arc:file-to-string
	    (merge-pathnames "color-passthrough.frag" *data-directory* )))
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
			 NIL))
    (%gl:use-program 0)
    (loop for shader-object in shader-list
       do (%gl:delete-shader shader-object))))


(defparameter *number-of-vertices* 8)

(defparameter +green-color+ '(0.0 1.0 0.0 1.0))
(defparameter +blue-color+  '(0.0 0.0 1.0 1.0))
(defparameter +red-color+   '(1.0 0.0 0.0 1.0))
(defparameter +grey-color+  '(0.8 0.8 0.8 1.0))
(defparameter +brown-color+ '(0.5 0.5 0.0 1.0))

(defparameter *vertex-data*
  (arc:create-gl-array-from-vector 
`#(
	+1.0  +1.0  +1.0  
	-1.0  -1.0  +1.0  
	-1.0  +1.0  -1.0  
	+1.0  -1.0  -1.0  

	-1.0  -1.0  -1.0  
	+1.0  +1.0  -1.0  
	+1.0  -1.0  +1.0  
	-1.0  +1.0  +1.0  

	,@+green-color+ 
	,@+blue-color+ 
	,@+red-color+ 
	,@+brown-color+ 

	,@+green-color+ 
	,@+blue-color+ 
	,@+red-color+ 
	,@+brown-color+ 

  )))

(defparameter *index-data*
  (arc::create-gl-array-of-unsigned-short-from-vector
   #(
     0  1  2 
     1  0  3 
     2  3  0 
     3  2  1 

     5  4  6 
     4  5  7 
     7  6  4 
     6  7  5 							
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

(defvar *elapsed-time*)

(defun null-scale ()
  (glm:vec3 1.0 1.0 1.0))

(defun static-uniform-scale ()
  (glm:vec3 4.0 4.0 4.0))

(defun static-nun-uniform-scale ()
  (glm:vec3 0.5 1.0 10.0))

(defun calc-lerp-factor (loop-duration)
  (let ((f-value (/ (mod *elapsed-time* loop-duration)
		      loop-duration)))
    (when (> f-value 0.5)
    	(setf f-value (- 1.0 f-value)))
    (coerce (* 2.0 f-value) 'single-float)))

;;TODO: why is it "pulsating"?
;; experimentation indicate that the implementation of mix might be wrong, as the pulsating
;; looks like we can see the inverse of the object for a moment (negative values must be
;; used for scale). Using 0.0 4.0 shows a neat shriking/growing animation
(defun dynamic-uniform-scale ()
  (let ((loop-duration 3.0))
    (glm:vec3 (glm:mix 1.0 4.0 (calc-lerp-factor loop-duration)))))

(defun dynamic-non-uniform-scale ()
  (let ((fx-loop-duration 3.0)
	(fz-loop-duration 5.0))
    (glm:vec3 (glm:mix 1.0 0.5 (calc-lerp-factor fx-loop-duration))
	      1.0
	      (glm:mix 1.0 10.0 (calc-lerp-factor fz-loop-duration)))))

;; Rotation code:
(defun null-rotation ()
  (glm:make-mat3 1.0))

(defun compute-angle-rad (loop-duration)
  (let ((scale (/ (* pi 2.0)
		  loop-duration))
	(curr-time-through-loop (mod *elapsed-time* loop-duration)))
    (coerce (* curr-time-through-loop scale)
	    'single-float)))

(defun rotate-x ()
  (let* ((ang-rad (compute-angle-rad 3.0))
	 (f-cos (cos ang-rad))
	 (f-sin (sin ang-rad))
	 (matrix (glm:make-mat3 1.0)))
    (glm:set-mat3 matrix 1 :y f-cos) (glm:set-mat3 matrix 2 :y (- f-sin)) 
    (glm:set-mat3 matrix 1 :z f-sin) (glm:set-mat3 matrix 2 :z f-cos)
    matrix
    ))

(defun rotate-y ()
  (let* ((ang-rad (compute-angle-rad 2.0))
	 (f-cos (cos ang-rad))
	 (f-sin (sin ang-rad))
	 (matrix (glm:make-mat3 1.0)))
    (glm:set-mat3 matrix 0 :x f-cos)     (glm:set-mat3 matrix 2 :x f-sin) 
    (glm:set-mat3 matrix 0 :z (- f-sin)) (glm:set-mat3 matrix 2 :z f-cos)
    matrix
    ))

(defun rotate-z ()
  (let* ((ang-rad (compute-angle-rad 2.0))
	 (f-cos (cos ang-rad))
	 (f-sin (sin ang-rad))
	 (matrix (glm:make-mat3 1.0)))
    (glm:set-mat3 matrix 0 :x f-cos) (glm:set-mat3 matrix 1 :x (- f-sin)) 
    (glm:set-mat3 matrix 0 :y f-sin) (glm:set-mat3 matrix 1 :y f-cos)
    matrix
    ))


;;TODO FIND TYPO
;; Rotate along arbitrary axis!
(defun rotate-axis (axis-x axis-y axis-z)
  (let* ((ang-rad (compute-angle-rad 0.4))
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

    matrix
    ))

(defvar *g-instance-list*)

(defun display ()
  (gl:clear-color 0 0 0.2 1)
  (gl:clear-depth 1.0)
  (gl:clear :color-buffer-bit :depth-buffer-bit)

  (%gl:use-program *program*)

  (gl:bind-vertex-array *vao*)

  (setf *elapsed-time* (float  (/ (sdl2:get-ticks) 1000.0)))
  (labels ((init-g-instance-list ()
	     ;; TODO: is it bad style not rely on all these functions to fetch
	     ;; *elapsed-time* globally?
	     (setf *g-instance-list*
		   (list
		    (list (null-rotation) (glm:vec3 +00.0 +00.0 -25.0))
		    (list (rotate-x) (glm:vec3 -5.0 -5.0 -25.0))
		    (list (rotate-y) (glm:vec3 -5.0 +5.0 -25.0))
		    (list (rotate-z) (glm:vec3 +5.0 +5.0 -25.0))
		    (list (rotate-axis 1.0 1.0 1.0) (glm:vec3 +5.0 -5.0 -25.0))
		    ))))
    (init-g-instance-list)

    )
  (let ((transform-matrix (glm:make-mat4 1.0)))
    ;; here we marry the identity matrix with the translation matrix AND
    ;; the scale matrix, so they can plant origins in a space AND grow them
    ;; at will!
    (loop for i from 0 below (length *g-instance-list*)
       for rotation-mat3 = (first (elt *g-instance-list* i))
       for translation-vec3 = (second (elt *g-instance-list* i)) do
	 (progn
	   ;; set transform-matrix to rotation
	   (setf transform-matrix (glm:mat4-from-mat3 rotation-mat3))  
	   ;; set translation in 'w'-column
	   (glm:set-mat4-col
	    transform-matrix 3 (glm:vec4-from-vec3 translation-vec3))

	   (gl:uniform-matrix
	    *model-to-camera-matrix-unif* 4 (vector transform-matrix) NIL)
	   (%gl:draw-elements
	    :triangles (gl::gl-array-size *index-data*) :unsigned-short 0))
	 ))



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
