(in-package #:arc-3.1)

(defvar *data-directory*
  (merge-pathnames #p "3-chapter/data/"
		   (asdf/system:system-source-directory :arcsynthesis)))

(defvar out *standard-output*)  (defvar dbg *debug-io*) (defvar err *error-output*)

(defparameter *vertex-positions* (gl:alloc-gl-array :float 12))
;;vertices followed by colors, also this time an equilateral triangle instead of a
;;isosceles
(defparameter *verts* #(0.0  0.25   0.0 1.0
			0.25 -0.183 0.0 1.0
		       -0.25 -0.183 0.0 1.0
			1.0  0.0   0.0 1.0
			0.0  1.0   0.0 1.0
			0.0  0.0   1.0 1.0)) 

(setf *vertex-positions* (arc::create-gl-array-from-vector *verts*))

(defparameter *position-buffer-object* nil) ; buffer object handle
(defparameter *x-offset* 0) (defparameter *y-offset* 0)

(defun compute-positions-offset ()	;wow, we don't even need *x-offset*, *y-offset* (lol!)
  "Return a list containing values which oscilate every 5 seconds"
  (let* ((loop-duration 5.0)
	 (scale (/ (* pi 2) loop-duration))
	 (elapsed-time (/ (sdl2:get-ticks) 1000.0))
	 ;; this is sorta cool, it in effect creates a value on the range [0,
	 ;; loop-duration) which is what we want: representing the 5 seconds circle!
	 (curr-time-through-loop (mod elapsed-time loop-duration)))
    ;; in the following "0.5" shrinks the cos/sin oscilation from 1 to -1 to 0.5 to -0.5
    ;; in effect creating a "circle of diameter 1 (from 0.5 to -0.5 = diameter 1) great
    ;; fun: substitute with cos,sin,tan and see how it beautifully moves in its patterns!
    (setf *x-offset* (* (cos (* curr-time-through-loop scale)) 0.5))
    (setf *y-offset* (* (sin (* curr-time-through-loop scale)) 0.5))))


(defparameter *offset-location* nil)

(defvar *program*)

(defun init-shader-program ()
  (let ((shader-list (list)))
    ;;oh c'mon how to make it local
    (push
     (arc:create-shader
      :vertex-shader
      (arc:file-to-string
       (merge-pathnames "position-offset.vert" *data-directory*)))
     shader-list)
    (push
     (arc:create-shader
      :fragment-shader
      (arc:file-to-string
       (merge-pathnames "standard.frag" *data-directory* )))
     shader-list)
    ;; TODO:fragment shader
    (setf *program* (arc:create-program shader-list))
    ;; TODO:"will return -1 if the uniform has no location ...? wut."
    ;; ah ok, when we give it a variable we didn't really declare in the
    ;; shader
    (setf *offset-location* (gl:get-uniform-location *program* "offset"))))


(defun set-up-opengl-state ()
  (setf *position-buffer-object* (first (gl:gen-buffers 1)))
  (%gl:bind-buffer :array-buffer *position-buffer-object*)
  ;; we want to change the buffer data, hence NOT :static-draw but :stream-draw
  ;; TODO: any visible performance penalties otherwise?
  (gl:buffer-data :array-buffer :stream-draw *vertex-positions*)
  (gl:bind-buffer :array-buffer 0)
  (gl:bind-buffer :array-buffer *position-buffer-object*)
  (%gl:enable-vertex-attrib-array 0) ; vertex array
  (%gl:enable-vertex-attrib-array 1) ; color array
  (%gl:vertex-attrib-pointer 0 4 :float :false 0 0)
  ;; this works :I, so does simply '48' as well :x
  (%gl:vertex-attrib-pointer 1 4 :float :false 0 (cffi:make-pointer 48)) 
  )


(defun rendering-code ()
  (gl:clear :color-buffer-bit)
  (gl:use-program *program*)

  (compute-positions-offset)
  ;;  (adjust-vertex-data) ; this time we use the shader!
  ;; now we can set some uniforms using the location "handle" *offset-location*!!  
  (%gl:uniform-2f *offset-location* *x-offset* *y-offset*)
  (%gl:draw-arrays :triangles 0 3))

(defun main ()
  (sdl2:with-init (:everything)
    (progn (setf *standard-output* out) (setf *debug-io* dbg) (setf *error-output* err))
    (sdl2:with-window (win :w 400 :h 400 :flags '(:shown :opengl))
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
	     )
	   (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
	     (sdl2:push-event :quit)))
	  (:quit () t)
	  (:idle ()
		 ;;main-loop:
		 (rendering-code)
		 (sdl2:gl-swap-window win) ; wow, this can be forgotten easily -.-
		 ))))))


