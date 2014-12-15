;; TODO: about

(in-package #:arc-8)

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
(defvar *p-object*)

(defun init ()
  (initialize-program)
  (let ((gimbal-names
	 (mapcar #'(lambda (xml) (merge-pathnames *data-dir* xml))
		 (list "LargeGimbal.xml"
		       "MediumGimbal.xml"
		       "SmallGimbal.xml"))))
    (loop for i below (length *gimbal-meshes*)
       for xmls in gimbal-names do
	 (setf (aref *gimbal-meshes* i) (framework:xml->mesh-obj xmls))))

  ;;TODO extend 'framework.lisp' to deal with <vao ...> xml-attributes
  ;; (setf *p-object* (framework:xml->mesh-obj (merge-pathnames *data-dir* "Ship.xml")))
  ;; so as to finally being able to load the the "Ship.xml" as a mesh to be rendered
  (setf *p-object* (framework:xml->mesh-obj (merge-pathnames *data-dir* "UnitCone.xml")))
 
  (gl:enable :cull-face)
  (%gl:cull-face :back)
  (%gl:front-face :cw) 

  (gl:viewport 0 0 500 500)

  (gl:enable :depth-test)
  (gl:depth-mask :true)
  (%gl:depth-func :lequal)
  (gl:depth-range 0.0 1.0)
  )


(defclass gimbal-angles ()
  ((angle-x :accessor angle-x :initform 0.0)
   (angle-y :accessor angle-y :initform 0.0)
   (angle-z :accessor angle-z :initform 0.0)))

(defparameter *angles* (make-instance 'gimbal-angles))

(defun draw-gimbal (matrix-stack e-axis base-color)

  (unless *draw-gimbal* (return-from draw-gimbal NIL))

  ;; TODO: more powerful WITH-TRANSFORM which finds transform keywords and repalces
  ;; them with transformation functions
  (glutil:with-transform (matrix-stack)

      (case e-axis
      	(:x #|do nothing|#)
      	(:y
      	 (glutil::rotate-z matrix-stack -90.0)
      	 (glutil::rotate-x matrix-stack 90.0)
      	 )
      	(:z
      	 (glutil::rotate-y matrix-stack 90.0)
      	 (glutil::rotate-x matrix-stack 90.0)
      	 ))

    (gl:use-program *program*)
    ;;set the base color for this object
    ;; 'count = 1' means: only a single variable will be modified. uniforms can
    ;; consist of an entire array
    (%gl:uniform-4f *base-color-unif*
		    (glm:vec. base-color :x)
		    (glm:vec. base-color :y)
		    (glm:vec. base-color :z)
		    (glm:vec. base-color :w))
    (gl:uniform-matrix *model-to-camera-matrix-unif* 4
		       (vector (glutil:top-ms matrix-stack)) NIL)

    (framework:render (glm:vec. *gimbal-meshes* e-axis))

    (gl:use-program 0)))

(defun draw ()
  (let ((curr-matrix (make-instance 'glutil:matrix-stack)))
    (glutil:with-transform (curr-matrix)
	:translate 0.0 0.0 -200.0
	;; this gimbal simulation uses the hierarchical model:
	
	;; Holy RAWR, this is awesome!!
	;; transform outer gimbal
	:rotate-x (angle-x *angles*)
	(draw-gimbal curr-matrix :x (glm:vec4 0.4 0.4 1.0 1.0))

	;; transform middle gimbal
	:rotate-y (angle-y *angles*)
	(draw-gimbal curr-matrix :y (glm:vec4 0.0 1.0 0.0 1.0))

	;; transform inner-most gimbal
	:rotate-z (angle-z *angles*) 
	(draw-gimbal curr-matrix :z (glm:vec4 1.0 0.3 0.3 1.0))

	(gl:use-program *program*)
	:scale 3.0 20.0 22.0
	:rotate-x -90.0
	;; set the base color for this object
        (%gl:uniform-4f *base-color-unif* 1.0 1.0 1.0 1.0)
	(gl:uniform-matrix *model-to-camera-matrix-unif* 4
			   (vector (glutil:top-ms curr-matrix)) NIL)
	;;render object call:
	(framework:render *p-object*)

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

(defconstant +standard-angle-increment+ 11.25)
(defconstant +small-angle-increment+ 9.0)
(defparameter *draw-gimbal* t)

(defun animation ()
  (let ((x (mod (+ (angle-x *angles*) .1) 360.0))
	(y (mod (- (angle-y *angles*) .2) 360.0))
	(z (mod (+ (angle-z *angles*) .0) 360.0)))
    (setf (angle-x *angles*) x)
    (setf (angle-y *angles*) y)
    (setf (angle-z *angles*) z)))

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
	     (incf (angle-y *angles*) +small-angle-increment+))
	   (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-d)
	     (decf (angle-y *angles*) +small-angle-increment+))

	   (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-w)
	     (incf (angle-x *angles*) +small-angle-increment+))
	   (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-s)
	     (decf (angle-x *angles*) +small-angle-increment+))

	   (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-q)
	     (incf (angle-z *angles*) +small-angle-increment+))
   	   (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-e)
	     (decf (angle-z *angles*) +small-angle-increment+))

	   (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-space)
	     (if *draw-gimbal*
		 (setf *draw-gimbal* NIL)
		 (setf *draw-gimbal* t))
	     (format t "Draw gimbal: ~a~%" *draw-gimbal*)
	     )

	   (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
	     (sdl2:push-event :quit)))
	  (:quit () t)
	  (:idle ()
		 ;;main-loop:
		 (display)
;		 (animation)
		 
                 (sdl2:gl-swap-window win) ; wow, this can be forgotten easily -.-
		 ))))))

