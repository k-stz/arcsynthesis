;; tell the compiler to not care about speed, use maximum type saftey and
;; give us maximum debug information
;; TODO: sbcl specifics?
(declaim (optimize (speed 0) (safety 3) (debug 3)))

;;; About: Shows the simplest lighting model: "lambertian reflectance" to compute the
;;;        colors at each primitives vertex then interpolate them over the surface
;;;        of the triangle (Gouroud Shading). See: the vertex shaders
;;;        The a "directional light source" is being used, this means that every vertex
;;;        on the screen receives the light at the same angle, the angle of incidence,
;;;        this light source usage makes sense when the light source is very far away
;;;        like the sun from earth. See: *light-direction*.
;;;        This tutorials camera may be controlled by clicking on the screen and
;;;        thereby moving it. Use the mousewheel to zoom in/out. See: *view-pole*

;;; REMEMBER: the lighting intensity is already given, and the light
;;;           direction IS ALREADY IN CAMERA SPACE, the shader doesn't
;;;           do anything regarding that
;;;
;;;           TWO vertex-shaders are used!
(in-package #:arc-9)

;; TODO: this might solve the problem:
;; (print (uiop/lisp-build:current-lisp-file-pathname)) ?
(defvar *data-directory*
  (merge-pathnames
   #p "9-chapter/data/" (asdf/system:system-source-directory :arcsynthesis)))

;;todo: fix this output to slime-repl solution
(defvar out *standard-output*)  (defvar dbg *debug-io*) (defvar err *error-output*)


;; now acts a lot like a C++-struct, (:conc-name NIL) ensures that the automatically
;; generated reader functions of a struct don't start with program-data-.. which is the
;; default behaviour but with nothing at all, NIL. A string could be specified after
;; :conc-name to get custom prefix reader string: (:conc-name "foo-") -> (foo-the-program
;; obj) This solution has been chosen, instead of (defclass program-data (...) :accessor
;; the-program) due to DEFCLASS considered overkill. Plus it has a nice print
;; representation by default.  hmmm TODO: (class-of <program-data-obj>) ==>
;; #<STRUCTURE-CLASS TEST>
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
	   (arc:file-to-string (merge-pathnames str-vertex-shader *data-directory*)))
	  shader-list)
    (push (arc:create-shader
	   :fragment-shader
	   (arc:file-to-string (merge-pathnames str-fragment-shader *data-directory*)))
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

    (setf projection-block
	  ;; TODO: get cl-opengl version containing this version
	  ;;(gl:get-uniform-block-index (the-program data) "Projection")
	  (cffi:with-foreign-string (s "Projection")
	    (%gl:get-uniform-block-index (the-program data) s)))
    (%gl:uniform-block-binding
     (the-program data) projection-block +projection-block-index+)
    data))



(defun initialize-program ()
  (setf *white-diffuse-color*
	(load-program "DirVertexLighting_PN.vert" "ColorPassthrough.frag"))
  (setf *vertex-diffuse-color*
	(load-program "DirVertexLighting_PCN.vert" "ColorPassthrough.frag")))


(defvar *plane-mesh*)
(defvar *cylinder-mesh*)

(defparameter *projection-uniform-buffer* 0)

(defun init ()
  (initialize-program)

  (setf *plane-mesh*
  	(framework:xml->mesh-obj (merge-pathnames *data-directory* "UnitPlane.xml")))

  (setf *cylinder-mesh*
  	(framework:xml->mesh-obj (merge-pathnames *data-directory* "UnitCylinder.xml")))

  (gl:enable :cull-face)
  (%gl:cull-face :back)
  (%gl:front-face :cw) 
  (gl:viewport 0 0 500 500)

  (gl:enable :depth-test)
  (gl:depth-mask :true)
  (%gl:depth-func :lequal)
  (gl:depth-range 0.0 1.0)
  (gl:enable :depth-clamp)

  (setf *projection-uniform-buffer* (first (gl:gen-buffers 1)))
  (gl:bind-buffer :uniform-buffer *projection-uniform-buffer*)
  (%gl:buffer-data :uniform-buffer #|sizeof(mat4):|# 64 (cffi:null-pointer) :dynamic-draw)

  ;;TODO: "bind the static buffer"
  (%gl:bind-buffer-range :uniform-buffer +projection-block-index+
			 *projection-uniform-buffer* 0 #|sizeof(mat4):|# 64)

  (gl:bind-buffer :uniform-buffer 0))



(defparameter *sphere-cam-rel-pos* (glm:vec3 90.0 -33.0 1.0) "Order: phi theta r")
(defparameter *cam-target* (glm:vec3 0.0 0.0 0.0))



(defparameter *light-direction* (glm:vec4 0.866 0.5 0.0 0.0))
(defparameter *draw-colored-cyl* t)

(defparameter *view-pole*
  (make-instance 'glutil:view-pole :cam-pos (glm:vec3 0.0 0.8 8.0)
		 ;; calculate trasformation relative to the look-pt
		 ;; for now changes calc-matrix behaviour
		 :trans-mode :camera-relative))



;; initialobjectdata position: 0.0 0.5 0.0
;;                   orientation: (quaternion 1.0 0.0 0.0 0.0)
(defparameter *objt-pole*
  (make-instance 'glutil:object-pole
		 :pos (glm:vec3 0.0 0.5 0.0)
		 :orient (glm:quaternion 1.0 0.0 0.0 0.0)))



(defun draw ()
  (let ((model-matrix (make-instance 'glutil:matrix-stack))
	(light-dir-camera-space))

    (glutil:set-matrix model-matrix (glutil:calc-matrix *view-pole*))

    
    (setf light-dir-camera-space
    	  (glm:vec4->vec3 (glm:mat*vec (glutil:top-ms model-matrix)
    				       *light-direction*)))
    
    (gl:use-program (the-program *white-diffuse-color*))
    ;; very convenient function, not only does it test the length of the input
    ;; (uniform-..1-4..-f) but it also accepts a vector as input!
    (gl:uniformfv (dir-to-light-unif *white-diffuse-color*) light-dir-camera-space)
    (gl:use-program (the-program *vertex-diffuse-color*))
    (gl:uniformfv (dir-to-light-unif *vertex-diffuse-color*) light-dir-camera-space)
    (gl:use-program 0)

    (glutil:with-transform (model-matrix)
	
	;; Render the ground plane
	(glutil:with-transform (model-matrix)
	    (gl:use-program (the-program *white-diffuse-color*))
	  :scale 5.0 1.0 5.0
	  ;;note how model-to-camera-matrix is mat4 and normal-model-to-camera-matrix is
	  ;;mat3!
	  (gl:uniform-matrix (model-to-camera-matrix-unif *white-diffuse-color*) 4
			     (vector (glutil:top-ms model-matrix)) NIL)

	  (gl:uniform-matrix (normal-model-to-camera-matrix-unif *white-diffuse-color*) 3
			     (vector (glm:mat4->mat3 (glutil:top-ms model-matrix))) NIL)

	  (gl:uniformfv
	   (light-intensity-unif *white-diffuse-color*) (glm:vec4 1.0 1.0 1.0 1.0))
	  (framework:render *plane-mesh*)
	  (gl:use-program 0))


      (glutil:apply-matrix model-matrix (glutil:calc-matrix *objt-pole*))
      ;; Render the Cylinder
      (if *draw-colored-cyl*
	  (glutil:with-transform (model-matrix)
	      (gl:use-program (the-program *vertex-diffuse-color*))

	      (gl:uniform-matrix (model-to-camera-matrix-unif *vertex-diffuse-color*) 4
				 (vector (glutil:top-ms model-matrix)) NIL)

	      (gl:uniform-matrix
	       (normal-model-to-camera-matrix-unif *vertex-diffuse-color*) 3
	       (vector (glm:mat4->mat3 (glutil:top-ms model-matrix))) NIL)

	      (gl:uniformfv
	       (light-intensity-unif *vertex-diffuse-color*) (glm:vec4 1.0 1.0 1.0 1.0))
	      (framework:render-mode *cylinder-mesh* "lit-color")
	  
	      (gl:use-program 0))

	  ;;else:
	  (glutil:with-transform (model-matrix)

	      (gl:use-program (the-program *white-diffuse-color*))
	    (gl:uniform-matrix (model-to-camera-matrix-unif *white-diffuse-color*) 4
			       (vector (glutil:top-ms model-matrix)) NIL)

	    (gl:uniform-matrix (normal-model-to-camera-matrix-unif *white-diffuse-color*) 3
			       (vector (glm:mat4->mat3 (glutil:top-ms model-matrix))) NIL)

	    (gl:uniformfv (light-intensity-unif *white-diffuse-color*)
			  (glm:vec4 1.0 1.0 1.0 1.0))
	    (framework:render-mode *cylinder-mesh* "lit")
	    (gl:use-program 0))))))

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


(defun mouse-rel-transform (xrel yrel)
  "Allow to look around with the mouse, just like in egoshooters."
  (glutil:rotate-vp :y xrel *view-pole*)
  (glutil:rotate-vp :x yrel *view-pole*))


(defun lmb-pressed-p (state)
  (= state 1))

(defun main ()
  (arc:with-main
    (sdl2:with-init (:everything)
      (progn (setf *standard-output* out) (setf *debug-io* dbg) (setf *error-output* err))
      (sdl2:with-window (win :w 500 :h 500 :flags '(:shown :opengl))
	(sdl2:with-gl-context (gl-context win)
	  ;; INIT code:
	  (init)
	  ;; TODO: callback for reshape; for now used to setup cam-to-clip-space matrix
	  (reshape 500.0 500.0)
	  (sdl2:with-event-loop (:method :poll)
	    (:mousewheel
	     (:y y)			; stores the vertical mousewheel motion

	     ;; zoom in/out
	     (when (= y 1)
	       (glutil:move-camera *view-pole*
				    (glutil:pole-direction
				     *view-pole*
				     (glm:vec3 0.0 0.0 -1.0)))
	       (format t "pos:~a~%~%"
		       (glm:round-obj (glutil:cam-pos *view-pole*) 0.001)))
	     (when (= y -1)
	       (glutil:move-camera *view-pole*
				    (glutil:pole-direction
				     *view-pole*
				     (glm:vec3 0.0 0.0 1.0)))
	       (format t "pos:~a~%~%"
		       (glm:round-obj (glutil:cam-pos *view-pole*) 0.001))))
	    
	    (:mousemotion
	     (:x x :y y :xrel xrel :yrel yrel :state state)
	     ;; and that's all we need to build the arc viewpole: xrel, yrel
	     ;; store the motion relative to the last event!
	     (format t "x:~a y:~a xrel:~a yrel:~a STATE:~a TRANS:~a~%" x y xrel yrel state
		     (glutil:trans-relative-to *view-pole*))
	     (when (lmb-pressed-p state)
	       (mouse-rel-transform xrel yrel)))
	    
	    (:keydown
	     (:keysym keysym)
     	     (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-t)
	       ;; switch through transforms for debugging purpose
	       (case (glutil:trans-relative-to *view-pole*)
		 (:1st-person (setf (glutil:trans-relative-to *view-pole*) :free-camera))
		 (:free-camera (setf (glutil:trans-relative-to *view-pole*)
				     :camera-relative))
		 (:camera-relative (setf (glutil:trans-relative-to *view-pole*)
					 :1st-person))))

	     (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-space)
	       (if *draw-colored-cyl*
		   (setf *draw-colored-cyl* NIL)
		   (setf *draw-colored-cyl* t)))
	     (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
	       (sdl2:push-event :quit)))
	    (:quit () t)
	    (:idle ()
		   ;; MAIN LOOP:

		   ;;rendering code:
		   (display)

		   ;;live editing enabled:
		   (arc:update-swank)
		   (sdl2:gl-swap-window win))))))))

