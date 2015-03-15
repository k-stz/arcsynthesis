(in-package #:arc-3.3)

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

(defparameter position-buffer-object nil) ; buffer object handle
(defparameter x-offset 0) (defparameter y-offset 0)


;; uniforms
(defparameter time-uniform 0.0)
(defparameter loop-duration-uniform 0)

(defvar *program*)

(defun init-shader-program ()
  (let ((shader-list (list)))
    ;;oh c'mon how to make it local
    (push
     (arc:create-shader
      :vertex-shader
      (arc:file-to-string
       (merge-pathnames "calc-offset.vert" *data-directory*)))
     shader-list)
    (push
     (arc:create-shader
      :fragment-shader
      (arc:file-to-string
       (merge-pathnames "calc-color.frag" *data-directory* )))
     shader-list)
    (let ( ; (loop-duration-uniform)
	  (frag-loop-duration-uniform))
      (setf *program* (arc:create-program shader-list))
      ;; here be uniform locations handlels
      (setf time-uniform (gl:get-uniform-location *program* "time"))
      (setf loop-duration-uniform (gl:get-uniform-location *program* "loop_duration"))
      (setf frag-loop-duration-uniform
	    (gl:get-uniform-location *program* "frag_loop_duration"))
      (%gl:use-program *program*)
      (%gl:uniform-1f loop-duration-uniform 5.0)
      (%gl:uniform-1f frag-loop-duration-uniform 3.0)
      
      (gl:use-program 0))))


(defun set-up-opengl-state ()
  (setf position-buffer-object (first (gl:gen-buffers 1)))
  (%gl:bind-buffer :array-buffer position-buffer-object)
  ;; we want to change the buffer data, hence NOT :static-draw but :stream-draw
  ;; TODO: any visible performance penalties otherwise?
  (gl:buffer-data :array-buffer :stream-draw *vertex-positions*)
  (gl:bind-buffer :array-buffer 0)
  (gl:bind-buffer :array-buffer position-buffer-object)
  (%gl:enable-vertex-attrib-array 0) ; vertex array-buffer
  (%gl:enable-vertex-attrib-array 1) ; color array-buffer
  (%gl:vertex-attrib-pointer 0 4 :float :false 0 0)
  ;; this works :I, so does simply '48' as well :x
  (%gl:vertex-attrib-pointer 1 4 :float :false 0 (cffi:make-pointer 48)) 
  )


(defun rendering-code ()
  ;;strange arcsynthesis repeadetly calls "glUseProgram" hmm
  ;;in the init code it finishes with (gl:use-program 0), and in this rendering code
  ;;arcsynthesis runs (gl:use-program program) then some code that needs it to be
  ;; set like: (gl:uniform-1f time-uniform ..) and then sets in to (gl:use-program 0)
  ;; every loop. This could be an indicator that many different shaders will be used?
  ;;  i.e.: TODO: make program object 'program' a global variable :I
  ;; AND: TODO: well this will solve my extremly repetitive code, by just passing
  ;;            the shader program of a certain chapter into it :I
  
  (gl:clear :color-buffer-bit)
  (gl:use-program *program*)
  (%gl:uniform-1f time-uniform (/ (sdl2:get-ticks) 1000.0))

  ;; this cool neat effect gets you thinking more clearly how the rendering happens
  (%gl:uniform-1f loop-duration-uniform 5.0)
  (%gl:draw-arrays :triangles 0 3)
  (%gl:uniform-1f loop-duration-uniform 2.5)
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


