;; TODO: capture resizing by reshape-event code!!!
;; TODO: WHAT is happening here??? how can it show the back side of the object just by
;;       moving eye up and down PERPENDICULAR to FRONT side???? this code must have
;;       a semantic error!

(in-package #:arc-4.3)

(defvar *data-directory*
  (merge-pathnames #p "4-chapter/data/" (asdf/system:system-source-directory :arcsynthesis)))

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
	0.0  1.0  1.0  1.0))
  


(setf *vertex-positions* (arc::create-gl-array-from-vector *verts*))

(defvar *position-buffer-object*) ; buffer object handle
(defvar *program*)
(defvar *offset-uniform*)

(defvar *matrix-uniform*)
(defparameter *perspective-matrix* nil)

(defparameter *frustum-scale* 0)

(defun init-shader-program ()
  (let ((shader-list (list)))
    ;;oh c'mon how to make it local
    (push (arc:create-shader
	   :vertex-shader
	   (arc:file-to-string
	    (merge-pathnames "matrix-perspective.vert" *data-directory*)))
	  shader-list)
    (push (arc:create-shader
    	   :fragment-shader
    	   (arc:file-to-string (merge-pathnames "standard-colors.frag" *data-directory* )))
    	  shader-list)
    (setf *program* (arc:create-program shader-list))
    (let ((s 1.0) (n 0.5) (f 3.0)) 	;frustum-scale, zNear, zFar
      (setf *matrix-uniform* (gl:get-uniform-location *program* "perspective_matrix"))
      (setf *perspective-matrix*
	    (make-array 16 :element-type 'single-float
			:initial-contents
			;; the matrix with its coefficients 'switches' and the perma 
			;; "on-switch" the 'w' component
			(list s 0.0 0.0 0.0                                     ;= x
			      0.0 s 0.0 0.0                                     ;= y
			      0.0 0.0 (/ (+ f n) (- n f)) (/ (* 2 f n) (- n f)) ;= z
			      0.0 0.0 -1.0 0.0)))                               ;= w

      (setf *offset-uniform* (gl:get-uniform-location *program* "offset"))
      (%gl:use-program *program*)
      (gl:uniform-matrix *matrix-uniform* 4 (vector *perspective-matrix*)))))


(defun set-up-opengl-state ()
  (setf *position-buffer-object* (first (gl:gen-buffers 1)))
  (%gl:bind-buffer :array-buffer *position-buffer-object*)
  (gl:buffer-data :array-buffer :stream-draw *vertex-positions*)
  (gl:bind-buffer :array-buffer 0)
  (gl:bind-buffer :array-buffer *position-buffer-object*)
  (%gl:enable-vertex-attrib-array 0) ; vertex array-buffer
  (%gl:enable-vertex-attrib-array 1) ; color array-buffer
  (%gl:vertex-attrib-pointer 0 4 :float :false 0 0)
  (%gl:vertex-attrib-pointer 1 4 :float :false 0 (/ (* 4 (length *verts*)) 2))

  (gl:enable :cull-face)
  (%gl:cull-face :back)
  (%gl:front-face :cw) ; help gl:cull-face decide who is a :back triangle
  (gl:viewport 0 0 500 500))


(defun rendering-code ()
  (gl:clear :color-buffer-bit)
  (%gl:uniform-2f *offset-uniform* 0.5 0.5)
  (%gl:draw-arrays :triangles 0 36))

(defun resize (win)
  (format t "resizing event emulation executed. ")
  ;; changing *frustum-scale* makes vertices grow fonder or ill of their origin :]
  (setf *frustum-scale* 1.0) ; TODO: hardcode baaaad
  (multiple-value-bind (w h) (sdl2:get-window-size win)
   (list w h)
   ;; this is quite cool: if width becomes bigger than h we effectively multiply the x
   ;; values with the ration h/w where w > h hence it fits less then 1 time in h and
   ;; the multiplication causes shrinkin!
   ;; on the other hand if width is smaller than h then h/w will create a value bigger
   ;; than 1 hence cause a x-enlarging effect!!
   (setf (aref *perspective-matrix* 0) (/ *frustum-scale* (/ w h))) ;coerce to 'single-float?
   (setf (aref *perspective-matrix* 5) *frustum-scale*) 
   (gl:uniform-matrix *matrix-uniform* 4 (vector *perspective-matrix*))
   (gl:viewport 0 0 w h)
   (format t "w: ~a h: ~a ~%" w h)))


(defparameter qux 0)
(defparameter eye-x 0.0)
(defparameter eye-y 1.0)

(defun change-eye ()
  ;; TODO: could just use the 'w' in *perspective-matrix* as it receives
  ;; just 1.0, so: (* 1.0 x) = x from the vertex and will be added to
  ;; the row of any vertex we want to modify by adding values!
  (let* ((matrix (make-array 16 :element-type 'single-float
			     :initial-contents *perspective-matrix*))
	 ;; (eye-x 1.0)
	 (eye-y 0.0)
	 (eye-z -1.0)
	 (loop-duration 5.0)
	 (scale (coerce (/ (* pi 2) loop-duration) 'single-float))
	 (elapsed-time (/ (sdl2:get-ticks) 1000.0))
	 (curr-time-through-loop (mod elapsed-time loop-duration)))
    ;;for experimentation:
    ;;TODO: strange flipping effect; hypothesis: triangle winding isn't on the
    ;;surface everywhere with :cw winding? hence some get culled they shouldn't
    ;;and at the point of flipping we see the backside of the triangle on the
    ;;inside of the prism
    ;;Or that's just the kind of distortion that occurs because the eye moves
    ;;but not the projection plane :I
    ;(setf eye-x (1-  (cos (* curr-time-through-loop scale))))
    (setf eye-y (1-  (sin (* curr-time-through-loop scale))))
    ;(setf eye-z (1-  (cos (* curr-time-through-loop scale))))


    (incf (aref matrix 0) eye-x)	;x
    (incf (aref matrix 5) eye-y)	;y
    (setf (aref matrix 14) (/ (aref matrix 14) (- eye-z)))
    (gl:uniform-matrix *matrix-uniform* 4 (vector matrix) :false)))

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
	     ;;(gl:disable :cull-face)
	     ;; TODO!!! HOW TO CATCH RESIZE EVENTS??
	     (resize win)
	     ;;      (gl:enable :depth-test)
	     ;; (gl:depth-mask :true)
	     ;; (%gl:depth-func :lequal)
	     ;; (%gl:depth-range 0.0 1.0)

					;(change-eye)
	     )
	   (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
	     (sdl2:push-event :quit)))
	  (:quit () t)
	  (:idle ()
		 ;;main-loop:
		 (rendering-code)
		 ;; nice experiments with change-eye 
		 (change-eye)
		 (sdl2:gl-swap-window win) ; wow, this can be forgotten easily -.-
		 ))))))
