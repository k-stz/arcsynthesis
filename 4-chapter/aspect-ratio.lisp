;; TODO: capture resizing by reshape-event code!!!

(in-package #:arc-4.3)

(defvar *glsl-directory*
  (merge-pathnames #p "4-chapter/" (asdf/system:system-source-directory :arcsynthesis)))
;;TODO: what with this garbage here >_>, or should I really build the habit of looking
;; at the terminal
(defvar out *standard-output*)  (defvar dbg *debug-io*) (defvar err *error-output*)

(defparameter *vertex-positions* nil)

(defparameter *verts* #(
;; vertices:
	 0.25   0.25  -1.25  1.0 
	 0.25  -0.25  -1.25  1.0 
	-0.25   0.25  -1.25  1.0 

	 0.25  -0.25  -1.25  1.0 
	-0.25  -0.25  -1.25  1.0 
	-0.25   0.25  -1.25  1.0 

	 0.25   0.25  -2.75  1.0 
	-0.25   0.25  -2.75  1.0 
	 0.25  -0.25  -2.75  1.0 

	 0.25  -0.25  -2.75  1.0 
	-0.25   0.25  -2.75  1.0 
	-0.25  -0.25  -2.75  1.0 

	-0.25   0.25  -1.25  1.0 
	-0.25  -0.25  -1.25  1.0 
	-0.25  -0.25  -2.75  1.0 

	-0.25   0.25  -1.25  1.0 
	-0.25  -0.25  -2.75  1.0 
	-0.25   0.25  -2.75  1.0 

	 0.25   0.25  -1.25  1.0 
	 0.25  -0.25  -2.75  1.0 
	 0.25  -0.25  -1.25  1.0 

	 0.25   0.25  -1.25  1.0 
	 0.25   0.25  -2.75  1.0 
	 0.25  -0.25  -2.75  1.0 

	 0.25   0.25  -2.75  1.0 
	 0.25   0.25  -1.25  1.0 
	-0.25   0.25  -1.25  1.0 

	 0.25   0.25  -2.75  1.0 
	-0.25   0.25  -1.25  1.0 
	-0.25   0.25  -2.75  1.0 

	 0.25  -0.25  -2.75  1.0 
	-0.25  -0.25  -1.25  1.0 
	 0.25  -0.25  -1.25  1.0 

	 0.25  -0.25  -2.75  1.0 
	-0.25  -0.25  -2.75  1.0 
	-0.25  -0.25  -1.25  1.0 

;; colors:

	0.0  0.0  1.0  1.0 
	0.0  0.0  1.0  1.0 
	0.0  0.0  1.0  1.0 

	0.0  0.0  1.0  1.0 
	0.0  0.0  1.0  1.0 
	0.0  0.0  1.0  1.0 

	0.8  0.8  0.8  1.0 
	0.8  0.8  0.8  1.0 
	0.8  0.8  0.8  1.0 

	0.8  0.8  0.8  1.0 
	0.8  0.8  0.8  1.0 
	0.8  0.8  0.8  1.0 

	0.0  1.0  0.0  1.0 
	0.0  1.0  0.0  1.0 
	0.0  1.0  0.0  1.0 

	0.0  1.0  0.0  1.0 
	0.0  1.0  0.0  1.0 
	0.0  1.0  0.0  1.0 

	0.5  0.5  0.0  1.0 
	0.5  0.5  0.0  1.0 
	0.5  0.5  0.0  1.0 

	0.5  0.5  0.0  1.0 
	0.5  0.5  0.0  1.0 
	0.5  0.5  0.0  1.0 

	1.0  0.0  0.0  1.0 
	1.0  0.0  0.0  1.0 
	1.0  0.0  0.0  1.0 

	1.0  0.0  0.0  1.0 
	1.0  0.0  0.0  1.0 
	1.0  0.0  0.0  1.0 

	0.0  1.0  1.0  1.0 
	0.0  1.0  1.0  1.0 
	0.0  1.0  1.0  1.0 

	0.0  1.0  1.0  1.0 
	0.0  1.0  1.0  1.0 
	0.0  1.0  1.0  1.0 
			))
  


(setf *vertex-positions* (arc::create-gl-array-from-vector *verts*))

(defparameter position-buffer-object nil) ; buffer object handle
(defparameter x-offset 0) (defparameter y-offset 0)
(defparameter program nil "program-object")
(defparameter offset-uniform 0)
(defparameter z-near-uniform 0)
(defparameter z-far-uniform 0)
(defparameter frustum-scale-uniform 0)

(defparameter matrix-uniform nil)
(defparameter perspective-matrix nil)

(defparameter frustum-scale 0)

(defun init-shader-program ()
  (let ((shader-list (list)))
    ;;oh c'mon how to make it local
    (push (arc:create-shader
	   :vertex-shader
	   (arc:file-to-string
	    (merge-pathnames "vs-matrix-projection.glsl" *glsl-directory*)))
	  shader-list)
    (push (arc:create-shader
    	   :fragment-shader
    	   (arc:file-to-string (merge-pathnames "fs.glsl" *glsl-directory* )))
    	  shader-list)
    (setf program (arc:create-program-and-return-it shader-list))
    (let ((s 1.0) (n 0.5) (f 3.0)	;frustum-scale, zNear, zFar
	  )
      (setf matrix-uniform (gl:get-uniform-location program "perspective_matrix"))
      (print matrix-uniform)
      (setf perspective-matrix
	    (make-array 16 :element-type 'single-float
			:initial-contents
			;; the matrix with its coefficients 'switches' and the perma 
			;; "on-switch" the 'w' component
			(list s 0.0 0.0 0.0                                     ;= x
			      0.0 s 0.0 0.0                                     ;= y
			      0.0 0.0 (/ (+ f n) (- n f)) (/ (* 2 f n) (- n f)) ;= z
			      0.0 0.0 -1.0 0.0)))                               ;= w

      (setf offset-uniform (gl:get-uniform-location program "offset"))
      (%gl:use-program program)
      ;; TODO: beware sb-cga
      ;; TODO: gl:uniform-matrix super useful! lookup implementation!
      (gl:uniform-matrix matrix-uniform 4 (vector perspective-matrix) :false))
    (loop for shader-object in shader-list
       do (%gl:delete-shader shader-object))))


(defun set-up-opengl-state ()
  (setf position-buffer-object (first (gl:gen-buffers 1)))
  (%gl:bind-buffer :array-buffer position-buffer-object)
  (gl:buffer-data :array-buffer :stream-draw *vertex-positions*)
  (gl:bind-buffer :array-buffer 0)
  (gl:bind-buffer :array-buffer position-buffer-object)
  (%gl:enable-vertex-attrib-array 0) ; vertex array-buffer
  (%gl:enable-vertex-attrib-array 1) ; color array-buffer
  (%gl:vertex-attrib-pointer 0 4 :float :false 0 0)
  (%gl:vertex-attrib-pointer 1 4 :float :false 0 (/ (* 4 (length *verts*)) 2))

  (gl:enable :cull-face)
  ;; cull-face doesn't seem to be all performance after all, it is a necessity
  ;; so we don't draw over certain triangles >_>
  ;; TODO: hm we still have this problem when drawing multiple primitives over
  ;; each other, no?
  (%gl:cull-face :back)
  (%gl:front-face :cw) ; help gl:cull-face decide who is a :back triangle
  (gl:viewport 0 0 500 500)
  )


(defun rendering-code ()
  (gl:clear :color-buffer-bit)
  (%gl:uniform-2f offset-uniform 0.5 0.5)
  (%gl:draw-arrays :triangles 0 36)
  )

(defun resize (win)
  (format t "resizing event emulation executed. ")

  (setf frustum-scale 1.0) ; TODO: hardcode baaaad
  (multiple-value-bind (w h) (sdl2:get-window-size win)
   (list w h)
   (setf (aref perspective-matrix 0) (/ frustum-scale (/ w h))) ;coerce to 'single-float?
   (setf (aref perspective-matrix 5) frustum-scale)
   (gl:uniform-matrix matrix-uniform 4 (vector perspective-matrix) :false)
   (gl:viewport 0 0 w h)
   (format t "Prism:\"Hi there\"~%")
    ))

(defun main ()
  (sdl2:with-init (:everything)
    (progn (setf *standard-output* out) (setf *debug-io* dbg) (setf *error-output* err))
    (sdl2:with-window (win :w 500 :h 500 :flags '(:shown :opengl :resizable))
      (sdl2:with-gl-context (gl-context win)
	(gl:clear-color 0 0 0.2 1)
	(gl:clear :color-buffer-bit)
        (set-up-opengl-state)
	(init-shader-program)
	(sdl2:with-event-loop (:method :poll)
	  (:keyup
	   (:keysym keysym)
	   (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-e)
	     ;;experimental code
	     ;(gl:disable :cull-face)
	     ;; TODO!!! HOW TO CATCH RESIZE EVENTS??
	     (resize win))
	   (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
	     (sdl2:push-event :quit)))
	  (:quit () t)
	  (:idle ()
		 ;;main-loop:
		 (rendering-code)
		 (sdl2:gl-swap-window win) ; wow, this can be forgotten easily -.-
		 ))))))
