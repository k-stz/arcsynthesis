;; TODO: about

(in-package #:arc-8.1)

;; TODO: this might solve the problem:
;; (print (uiop/lisp-build:current-lisp-file-pathname)) ?
(defvar *glsl-directory*
  (merge-pathnames #p "8-chapter/" (asdf/system:system-source-directory :arcsynthesis)))
 (defvar *data-dir*
   (merge-pathnames #p "data/" *glsl-directory*))

;;todo: fix this output to slime-repl solution
(defvar out *standard-output*)  (defvar dbg *debug-io*) (defvar err *error-output*)

(defvar *program*)
(defvar *model-to-camera-matrix-unif*)
(defvar *camera-to-clip-matrix-unif*)
(defvar *base-color-unif*)

(defparameter *camera-to-clip-matrix* (glm:make-mat4 0.0))
(defparameter *frustum-scale* (glutil::calc-frustum-scale 20.0)) 

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
	    (merge-pathnames "color-mult-uniform.frag" *glsl-directory* )))
    	  shader-list)
    (setf *program* (arc:create-program-and-return-it shader-list)))

  (setf *model-to-camera-matrix-unif*
	(gl:get-uniform-location *program* "model_to_camera_matrix"))
  (setf *camera-to-clip-matrix-unif*
	(gl:get-uniform-location *program* "camera_to_clip_matrix"))
  (setf *base-color-unif*
	(gl:get-uniform-location *program* "base_color"))

  (format t "a:~a b:~a~%" *model-to-camera-matrix-unif* *camera-to-clip-matrix-unif*)

  (let ((fz-near 1.0)
	(fz-far 600.0))
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
  (%gl:use-program 0))


(defvar *gimbal-meshes* (make-array 3 :initial-element NIL))
(defvar *ship-vao*) ;; TODO: for test, remove and use *p-object* once it works

(defun init ()
  (initialize-program)

  (setf *ship-vao* (framework:ship-xml->vao (merge-pathnames *data-dir* "Ship.xml")))
 
  (gl:enable :cull-face)
  (%gl:cull-face :back)
  (%gl:front-face :cw) 

  (gl:viewport 0 0 500 500)

  (gl:enable :depth-test)
  (gl:depth-mask :true)
  (%gl:depth-func :lequal)
  (gl:depth-range 0.0 1.0)
  )

(defparameter *orientation* (glm:quaternion 1.0 0.0 0.0 0.0))

(defun draw ()
  (let ((curr-matrix (make-instance 'glutil:matrix-stack)))
    (glutil:with-transform (curr-matrix)
	:translate 0.0 0.0 -200.0
	;; here be the code deviating from gimbal-lock.lisp:

	;:translate 30.0 0.0 0.0
	:apply-matrix (glm:mat4-cast *orientation*)
	
	(gl:use-program *program*)
	:scale 3.0 3.0 3.0
	:rotate-x -90.0
        (%gl:uniform-4f *base-color-unif* 1.0 1.0 1.0 1.0)
	(gl:uniform-matrix *model-to-camera-matrix-unif* 4
			   (vector (glutil:top-ms curr-matrix)) NIL)
	
	(framework:render-ship *ship-vao*) 

	(gl:use-program 0))))

(defun display ()
  (gl:clear-color 0.0 0.0 0.2 1)
  (gl:clear-depth 1.0)
  (gl:clear :color-buffer-bit :depth-buffer-bit)

  (draw)
  )

(defparameter *fz-near* 1.0)
(defparameter *fz-far* 1000.0)

(defun reshape (w h)
  (glm:set-mat4 *camera-to-clip-matrix* 0 :x
		(* *frustum-scale* (/ h w)))
  (glm:set-mat4 *camera-to-clip-matrix* 1 :y *frustum-scale*)

  (%gl:use-program *program*)
  (gl:uniform-matrix *camera-to-clip-matrix-unif* 4 (vector *camera-to-clip-matrix*)
		     NIL)
  (%gl:use-program 0)

  (%gl:viewport 0 0 w h))




(defparameter *right-multiply-p* t) ; switch inside the OFFSET-ORIENTATION function

(defun offset-orientation (vec3-axis ang-deg)
  (let* ((vec3-axis (sb-cga:normalize vec3-axis))
	 (f-quat-offset
	  (glm:make-quat
	   ang-deg ((glm:vec. vec3-axis :x)
		    (glm:vec. vec3-axis :y)
		    (glm:vec. vec3-axis :z)))))

    ;; Check out this sweet treat right here:
    ;; 1. (quat* *orientation* f-quat-offset) => represent the f-quat-offset inside *orientation*
    ;; this means that the orientation is the base coordinate system and the f-quat-offset
    ;; is being represented relative to that base orientation ===> current orientation relative transformation
    ;; 2. (quat* f-quat-offset *orientation*) ==> f-quat-offset is a simple rotation about a
    ;; base-vector, the complex orientation accumulating in *orientation* will be represented
    ;; slightly "offset" by this base-vector deviation represented by f-quat-offset, somewhat like
    ;; an ===> base coordinate system "absolute" transformation (if we consider the ident-matrix coordianate
    ;; system as an "absolute" coordinate system, this only works seem to work because the provided
    ;; vec3-axis are either one of the three base-vectors: 1 0 0, 0 1 0, 0 0 1
    (if *right-multiply-p*
  	(setf *orientation* (glm:quat* *orientation* f-quat-offset))
  	(setf *orientation* (glm:quat* f-quat-offset *orientation*)))

    (setf *orientation* (glm:quat-normalize *orientation*))
    )
  )

(defconstant +standard-angle-increment+ 11.25)
(defconstant +small-angle-increment+ 9.0)

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
	   (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-w)
	     (offset-orientation (glm:vec3 1.0 0.0 0.0) +small-angle-increment+)
	     )
	   (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-s)
	     (offset-orientation (glm:vec3 1.0 0.0 0.0) (- +small-angle-increment+))
	     )

	   (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-a)
	     (offset-orientation (glm:vec3 0.0 0.0 1.0) +small-angle-increment+)
	     )
	   (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-d)
	     (offset-orientation (glm:vec3 0.0 0.0 1.0) (- +small-angle-increment+))
	     )

	   (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-q)
	     (offset-orientation (glm:vec3 0.0 1.0 0.0) +small-angle-increment+)
	     )
   	   (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-e)
	     (offset-orientation (glm:vec3 0.0 1.0 0.0) (- +small-angle-increment+))
	     )

	   (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-space)
	     (if *right-multiply-p*
		 (setf *right-multiply-p* nil)
		 (setf *right-multiply-p* t))
	     )

	   (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
	     (sdl2:push-event :quit)))
	  (:quit () t)
	  (:idle ()
		 ;;main-loop:
		 (display)
		 
                 (sdl2:gl-swap-window win) ; wow, this can be forgotten easily -.-
		 ))))))
