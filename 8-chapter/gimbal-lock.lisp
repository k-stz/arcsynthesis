;; TODO: about

(in-package #:arc-8)

;; TODO: this might solve the problem:
;; (print (uiop/lisp-build:current-lisp-file-pathname)) ?
(defvar *glsl-directory*
  (merge-pathnames #p "8-chapter/" (asdf/system:system-source-directory :arcsynthesis)))
;; (defvar *data-dir*
;;   (merge-pathnames #p "data/" *glsl-directory*))

;;todo: fix this output to slime-repl solution
(defvar out *standard-output*)  (defvar dbg *debug-io*) (defvar err *error-output*)

(defclass program-data ()
  ((the-program :accessor the-program)
   (model-to-world-matrix-unif :accessor model-to-world-matrix-unif)
   (world-to-camera-matrix-unif :accessor world-to-camera-matrix-unif)
   (camera-to-clip-matrix-unif :accessor camera-to-clip-matrix-unif)
   (base-color-unif :accessor base-color-unif)))


;;program-data
;; (defvar *uniform-color*)
;; (defvar *object-color*)
;; (defvar *uniform-color-tint*)

(defun load-program (str-vertex-shader str-fragment-shader)
  "Create program-data object from shader strings. Hardcoded uniform reference."
  (let ((shader-list (list))
	(data (make-instance 'program-data)))
    (push (arc:create-shader
	   :vertex-shader
	   (arc:file-to-string (merge-pathnames str-vertex-shader *glsl-directory*)))
	  shader-list)
    (push (arc:create-shader
	   :fragment-shader
	   (arc:file-to-string (merge-pathnames str-fragment-shader *glsl-directory*)))
    	  shader-list)
    (setf (the-program data) (arc:create-program shader-list))
    ;; hard-coding time: also this should undergo test if assignment was successful
    (setf (model-to-world-matrix-unif data)
	  (gl:get-uniform-location (the-program data) "model_to_world_matrix"))
    (setf (world-to-camera-matrix-unif data)
	  (gl:get-uniform-location (the-program data) "world_to_camera_matrix"))
    (setf (camera-to-clip-matrix-unif data)
	  (gl:get-uniform-location (the-program data) "camera_to_clip_matrix"))
    ;; TODO: if uniform doesn't really exist in shader, wasn't opengl lenient about it?
    (setf (base-color-unif data)
	  (gl:get-uniform-location (the-program data) "base_color"))
    data))


;; (defun initialize-program ()
;;   (setf *uniform-color*
;; 	(load-program "pos-only-world-transform.vert" "color-uniform.frag"))
;;   (setf *object-color*
;;   	(load-program "pos-color-world-transform.vert" "color-passthrough.frag"))
;;   (setf *uniform-color-tint*
;;   	(load-program "pos-color-world-transform.vert" "color-mult-uniform.frag")))


(defparameter *cone-mesh* nil)
(defparameter *cylinder-mesh* nil)
(defparameter *cube-tint-mesh* nil)
(defparameter *cube-color-mesh* nil)
(defparameter *plane-mesh* nil)

;; (defun init-meshes ()
;;   (setf *cone-mesh*
;; 	(framework::xml->mesh-obj (merge-pathnames *data-dir* "UnitConeTint.xml")))
;;   (setf *cylinder-mesh*
;; 	(framework::xml->mesh-obj (merge-pathnames *data-dir* "UnitCylinderTint.xml")))
;;   (setf *cube-tint-mesh*
;; 	(framework::xml->mesh-obj (merge-pathnames *data-dir* "UnitCubeTint.xml")))
;;   (setf *cube-color-mesh*
;; 	(framework::xml->mesh-obj (merge-pathnames *data-dir* "UnitCubeColor.xml")))
;;   (setf *plane-mesh*
;; 	(framework::xml->mesh-obj (merge-pathnames *data-dir* "UnitPlane.xml"))))

(defun init ()
	;;(initialize-program)
	;;(init-meshes)

	;; TODO: why doesn't this seem to affect the unit-plane when it is rotated 360?
	;; this gotta be a pernicious bug, swapping the z-axis so that the winding order is
	;; always clock-wise?
	(gl:enable :cull-face)
	(%gl:cull-face :back)
	(%gl:front-face :cw) 

	(gl:viewport 0 0 500 500)

	(gl:enable :depth-test)
	(gl:depth-mask :true)
	(%gl:depth-func :lequal)
	(gl:depth-range 0.0 1.0)
	)





(defun draw ()
  ;; 'display'-code
  )


(defun display ()
  (gl:clear-color 0.0 0.0 0.2 1)
  (gl:clear-depth 1.0)
  (gl:clear :color-buffer-bit :depth-buffer-bit)

  (draw)
  
  )

(defparameter *fz-near* 1.0)
(defparameter *fz-far* 1000.0)

(defun reshape (w h)
  ;; for now where we set the camera-to-clip perspective-matrix for the shaders
  ;; (let ((pers-matrix (make-instance 'glutil:matrix-stack)))
  ;;   (glutil:perspective pers-matrix 45.0 (/ w h) *fz-near* *fz-far*)
  ;;   ;; set camera-matrix for all programs
  ;;   (%gl:use-program (the-program *uniform-color*))
  ;;   (gl:uniform-matrix (camera-to-clip-matrix-unif *uniform-color*) 4
  ;; 		       (vector (glutil:top-ms pers-matrix)) NIL)
  ;;   (gl:use-program (the-program *object-color*))
  ;;   (gl:uniform-matrix (camera-to-clip-matrix-unif *object-color*) 4
  ;; 		       (vector (glutil:top-ms pers-matrix)) NIL)
  ;;   (gl:use-program (the-program *uniform-color-tint*))
  ;;   (gl:uniform-matrix (camera-to-clip-matrix-unif *uniform-color-tint*) 4
  ;; 		       (vector (glutil:top-ms pers-matrix)) NIL)
  ;;   (%gl:use-program 0))
  (%gl:viewport 0 0 w h))

(defun main ()
  (sdl2:with-init (:everything)
    (progn (setf *standard-output* out) (setf *debug-io* dbg) (setf *error-output* err))
    (sdl2:with-window (win :w 500 :h 500 :flags '(:shown :opengl :resizable))
      (sdl2:with-gl-context (gl-context win)
	;; INIT code:
	(init)
	;; TODO: callback for reshape; for now used to setup cam-to-clip-space matrix
	(reshape 500.0 500.0)
	(sdl2:with-event-loop (:method :poll)
	  (:keydown
	   (:keysym keysym)
	   ;; TODO: capture in macro
	   (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-a)
	     )
	   (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-d)
	     )

	   (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-w)
	     )
	   (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-s)
	     )

	   (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-e)
	     )
   	   (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-q)
	     )

	   (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-space)
	     (print "space pressed."))

	   (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
	     (sdl2:push-event :quit)))
	  (:quit () t)
	  (:idle ()
		 ;;main-loop:
		 (display)
		 
                 (sdl2:gl-swap-window win) ; wow, this can be forgotten easily -.-
		 ))))))

