;; TODO: about


;; REMEMBER: the lighting intensity is already given, and the light
;;           direction IS ALREADY IN CAMERA SPACE, the shader doesn't
;;           do anything regarding that
;;
;;           TWO vertex-shaders are used!


(in-package #:arc-9)

;; TODO: this might solve the problem:
;; (print (uiop/lisp-build:current-lisp-file-pathname)) ?
;; TODO: they're identical in all following tutorials? Then remove *glsl-directory*
(defvar *glsl-directory*
  (merge-pathnames #p "9-chapter/data/" (asdf/system:system-source-directory :arcsynthesis)))
(defvar *data-dir* *glsl-directory*) 

;;todo: fix this output to slime-repl solution
(defvar out *standard-output*)  (defvar dbg *debug-io*) (defvar err *error-output*)


;; now acts a lot like a C++-struct, (:conc-name NIL) ensures that the automatically
;; reader functions of a struct don't start with program-data-.. which is the default
;; behaviour but with nothing at all, NIL. A string could be specified after :conc-name
;; to get custom prefix reader string: (:conc-name "foo-") -> (foo-the-program obj)
;; This solution has been chosen, instead of
;; (defclass program-data (...) :accessor the-program)
;; due to DEFCLASS considered overkill. Plus it has a nice print representation by
;; default.  hmmm TODO: (class-of <program-data-obj>) ==> #<STRUCTURE-CLASS TEST>
(defstruct (program-data (:conc-name NIL)) ; don't hyphenate struct readers
  the-program

  dir-to-light-unif
  light-intensity-unif
  
  model-to-camera-matrix-unif
  normal-model-to-camera-matrix-unif)

(defvar *white-diffuse-color*)
(defvar *vertex-diffuse-color*)


(defconstant +projection-block-index+ 2)

(defun load-program (str-vertex-shader str-fragment-shader)
  "Create program-data object from shader strings. Hardcoded uniform reference."
  (let ((shader-list (list))
	(data (make-program-data))
	(projection-block))
    (push (arc:create-shader
	   :vertex-shader
	   (arc:file-to-string (merge-pathnames str-vertex-shader *glsl-directory*)))
	  shader-list)
    (push (arc:create-shader
	   :fragment-shader
	   (arc:file-to-string (merge-pathnames str-fragment-shader *glsl-directory*)))
    	  shader-list)
    ;; settings slots of program-object:
    (setf (the-program data) (arc:create-program shader-list))
    (setf (model-to-camera-matrix-unif data)
	  (gl:get-uniform-location (the-program data) "modelToCameraMatrix"))
    (setf (normal-model-to-camera-matrix-unif data)
	  (gl:get-uniform-location (the-program data) "normalModelToCameraMatrix"))
    (setf (dir-to-light-unif data)
	  (gl:get-uniform-location (the-program data) "dirToLight"))
    (setf (light-intensity-unif data)
	  (gl:get-uniform-location (the-program data) "lightIntensity"))
    (cffi:with-foreign-string (s "Projection")
      ;; TODO: still "gl:" update needed, checkout at newest cl-opengl
      (setf projection-block (%gl:get-uniform-block-index (the-program data) s)))
    (%gl:uniform-block-binding (the-program data) projection-block +projection-block-index+)
    data))



(defun initialize-program ()
  (setf *white-diffuse-color* (load-program "DirVertexLighting_PN.vert" "ColorPassthrough.frag"))
  (setf *vertex-diffuse-color* (load-program "DirVertexLighting_PCN.vert" "ColorPassthrough.frag")))


(defvar *plane-mesh*)

(defparameter *projection-uniform-buffer* 0)

(defun init ()
  (initialize-program)

  (setf *plane-mesh* (framework:xml->mesh-obj (merge-pathnames *data-dir* "simple-unit-plane.xml")))

  
  (gl:enable :cull-face)
  (%gl:cull-face :back)
  (%gl:front-face :cw) 
;;  (gl:viewport 0 0 500 500)

  (gl:enable :depth-test)
  (gl:depth-mask :true)
  (%gl:depth-func :lequal)
  (gl:depth-range 0.0 1.0)
  (gl:enable :depth-clamp)

  (setf *projection-uniform-buffer* (first (gl:gen-buffers 1)))
  (gl:bind-buffer :uniform-buffer *projection-uniform-buffer*)
  ;; TODO: revisit
  (%gl:buffer-data :uniform-buffer #|sizeof(mat4):|# 64 (cffi:null-pointer) :dynamic-draw)

  ;;TODO: "bind the static buffer"
  (%gl:bind-buffer-range :uniform-buffer +projection-block-index+ *projection-uniform-buffer* 0
			 #|sizeof(mat4):|# 64)

  (gl:bind-buffer :uniform-buffer 0))



(defparameter *sphere-cam-rel-pos* (glm:vec3 90.0 0.0 66.0) "Order: phi theta r")
(defparameter *cam-target* (glm:vec3 0.0 0.4 0.0))

(defun resolve-cam-position ()
  "Spherical coordinates stored in *sphere-cam-rel-pos* are transformed to Euclidean
geometry coordinates and returned as a position vector."
  (let* ((phi (framework:deg-to-rad (glm:vec. *sphere-cam-rel-pos* :x)))
	 (theta (framework:deg-to-rad (+ (glm:vec. *sphere-cam-rel-pos* :y)
					 90.0)))
	 (sin-theta (sin theta))
	 (cos-theta (cos theta))
	 (cos-phi (cos phi))
	 (sin-phi (sin phi))
	 ;;explanation in "world-with-ubo.lisp" in the chapter 7 directory
	 (dir-to-camera (glm:vec3 (* sin-theta cos-phi)
	 			  cos-theta
	 			  (* sin-theta sin-phi))))
    ;; Once the direction is set, we need to blow it up by multiplying the
    ;; "euclidean" vector by 'r' which is the :z value of *sphere-cam-rel-pos*
    ;; and add it to the target the camera is supposed to look at, thereby creating
    ;; a geometrical dependent positions of the camera to the *cam-target*
    (sb-cga:vec+ (sb-cga:vec* dir-to-camera (glm:vec. *sphere-cam-rel-pos* :z))
		 *cam-target*)))

(defun calc-look-at-matrix (camera-pt look-pt up-pt)
  ;; resolve-cam-position. negating the two yields the direction of the camera "through"
  ;; look-pt. Unit vector, through normalize, is needed as we will use look-dir
  ;; directly as the axis of the look-at-matrix, so as to prevent scaling!
  ;; With this we already have our Z axis
  (let* ((look-dir (sb-cga:normalize (sb-cga:vec- look-pt camera-pt)))
	 ;; since we only want a direction, we normalize the vector
	 ;; using up-dir we have a plane from which we can yield the perpendicular x-axis
	 ;; and from that the perpendicular y-axis (cross-product z-axis x-axis)
	 (up-dir (sb-cga:normalize up-pt))
	 ;; cross-product returns the vector perpendicular to the plane formed
	 ;; by two vectors:
	 ;; (sb-cga:cross-product (glm:vec3 1.0 0.0 0.0) (glm:vec3 0.0 1.0 0.0))
	 ;; ==> #(0.0 0.0 1.0)	 
	 (right-dir (sb-cga:normalize (sb-cga:cross-product look-dir up-dir)))
	 (perp-up-dir (sb-cga:cross-product right-dir look-dir))

	 (rot-mat (glm:make-mat4 1.0))
	 (trans-mat (glm:make-mat4 1.0)))

    (glm:set-mat4-col rot-mat 0 (glm:vec4-from-vec3 right-dir 0.0))
    (glm:set-mat4-col rot-mat 1 (glm:vec4-from-vec3 perp-up-dir 0.0))
    ;; the look-dir must be negated, or it wouldn't be a rotation of the original
    ;; identity matrix. As it currently would be a transform that couldn't arise
    ;; from rotating the original coordinate system (one axis is moved without
    ;; moving another by _the same_ angle). This must be a problem that arises from
    ;; using the cross-product, which doesn't seem to return the perpendicular axis
    ;; needed (there are always two perpendicular direction vectors for a plane)
    (glm:set-mat4-col rot-mat 2 (glm:vec4-from-vec3 (glm:vec- look-dir) 0.0))

    ;; TODO: why transpose it eventually? Maybe because it is col-major and setting
    ;; column-wise and transpose is more efficient than just setting the rows
    ;; with discontiguous indices
    (setf rot-mat (sb-cga:transpose-matrix rot-mat))

    ;; oh, its just a translation matrix putting the camera-pt into origin! (and thereby
    ;; offsetting every position sent through this matrix by the camera pos
    (glm:set-mat4-col trans-mat 3 (glm:vec4-from-vec3 (glm:vec- camera-pt) 1.0))

    ;;return rotmat * transmat;
    (sb-cga:matrix* rot-mat trans-mat)))


(defparameter *orientation* (glm:quaternion 1.0 0.0 0.0 0.0))


(defun draw ()
  (let ((model-matrix (make-instance 'glutil:matrix-stack))

	;;NEXT-TODO: get something on the screen look up ViewPole.CalcMatrix();
	(cam-pos (resolve-cam-position))
	(cam-matrix (make-instance 'glutil:matrix-stack)))

    (glutil:set-matrix cam-matrix (calc-look-at-matrix cam-pos *cam-target* (glm:vec3 0.0 1.0 0.0)))
    ;;TODO: lightdircamera = model-matrix.top() * light-direction;

    (gl:use-program (the-program *white-diffuse-color*))


    
    (glutil:with-transform (model-matrix)
	(gl:uniform-matrix (model-to-camera-matrix-unif *white-diffuse-color*) 4
			   (vector (glutil:top-ms model-matrix)) NIL)
      :translate -1.0 -1.0 -1.0
      :scale 1000.0 1.0 1000.0
      (framework:render *plane-mesh*))
    )

    
  )

(defun display ()
  (gl:clear-color 0.0 0.0 0.2 1)
  (gl:clear-depth 1.0)
  (gl:clear :color-buffer-bit :depth-buffer-bit)

  (draw))

(defparameter *fz-near* 1.0)
(defparameter *fz-far* 1000.0)

(defun reshape (w h)
  (let ((pers-matrix (make-instance 'glutil:matrix-stack)))
    (glutil:perspective pers-matrix 45.0 (/ w h) *fz-near* *fz-far*)

    (gl:bind-buffer :uniform-buffer *projection-uniform-buffer*)
    (gl:buffer-sub-data :uniform-buffer
		      (arc:create-gl-array-from-vector (glutil:top-ms pers-matrix)))
    (gl:bind-buffer :uniform-buffer 0))
  (%gl:viewport 0 0 w h))



(defconstant +standard-angle-increment+ 11.25)
(defconstant +small-angle-increment+ 9.0)


(defun main ()
  (arc::with-main
    (sdl2:with-init (:everything)
      (progn (setf *standard-output* out) (setf *debug-io* dbg) (setf *error-output* err))
      (sdl2:with-window (win :w 500 :h 500 :flags '(:shown :opengl))
	(sdl2:with-gl-context (gl-context win)
	  ;; INIT code:
	  (init)
	  ;; TODO: callback for reshape; for now used to setup cam-to-clip-space matrix
	  (reshape 500.0 500.0)
	  (sdl2:with-event-loop (:method :poll)
	    (:keydown
	     (:keysym keysym)
	     ;; TODO: capture in macro
	     (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-q)
	       )
	     (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-w)
	       )
	     (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-e)
	       )

	     ;; rotate camera horizontally around target
	     (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-j)
	       (decf (glm:vec. *sphere-cam-rel-pos* :x) 1.125))
	     (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-l)
	       (incf (glm:vec. *sphere-cam-rel-pos* :x) 1.125))
	     ;; rotate cam vertically around target
	     (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-i)
	       (decf (glm:vec. *sphere-cam-rel-pos* :y) 1.125))
	     (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-k)
	       (incf (glm:vec. *sphere-cam-rel-pos* :y) 1.125))
	     ;; zoom camera in/out of target
	     (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-u)
	       (decf (glm:vec. *sphere-cam-rel-pos* :z) 1.5))
	     (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-o)
	       (incf (glm:vec. *sphere-cam-rel-pos* :z) 1.5))


	     (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-space)
	       )

	     (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
	       (sdl2:push-event :quit)))
	    (:quit () t)
	    (:idle ()
		   ;; MAIN LOOP:

		   ;; preventing special cases (camera transformation):
		   (setf (glm:vec. *sphere-cam-rel-pos* :y)
			 (glm:clamp (glm:vec. *sphere-cam-rel-pos* :y) -78.75 1.0))
		   (setf (glm:vec. *cam-target* :y)
			 (if (> (glm:vec. *cam-target* :y) 0.0)
			     (glm:vec. *cam-target* :y)
			     0.0))
		   ;; can't zoom in "through" the floor with camera
		   (setf (glm:vec. *sphere-cam-rel-pos* :z)
			 (if (> (glm:vec. *sphere-cam-rel-pos* :z) 5.0)
			     (glm:vec. *sphere-cam-rel-pos* :z)
			     5.0))		 

		   ;;rendering code:
		   (display)

		   ;;live editing enabled:
		   (arc::update-swank)
		   
		   (sdl2:gl-swap-window win))))))))



