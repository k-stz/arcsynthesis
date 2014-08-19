(in-package #:arc-3.3)

(defvar *glsl-directory*
  (merge-pathnames #p "3-chapter/" (asdf/system:system-source-directory :arcsynthesis)))
;;TODO: what with this garbage here >_>, or should I really build the habit of looking
;; at the terminal
(defvar out *standard-output*)  (defvar dbg *debug-io*) (defvar err *error-output*)

(defparameter *vertex-positions* (gl:alloc-gl-array :float 12))
;;vertecies followed by colors, also this time an equilateral triangle instead of a isosceles
(defparameter *verts* #(0.0  0.25   0.0 1.0
			0.25 -0.183 0.0 1.0
		       -0.25 -0.183 0.0 1.0
			1.0  0.0   0.0 1.0
			0.0  1.0   0.0 1.0
			0.0  0.0   1.0 1.0)) 

(setf *vertex-positions* (arc::create-gl-array-from-vector *verts*))

(defparameter position-buffer-object nil) ; buffer object handle
(defparameter x-offset 0) (defparameter y-offset 0)

(defun adjust-vertex-data ()
  ;; like Java's STRING passing arround simple-vectors doesn't create copies,
  ;; they all point to the same vector => changing one chngers the simple-vector
  ;; for all
  (let ((new-data (make-array (length *verts*)
			      :initial-contents *verts*)))
    ;; TODO: better way to loop through this simple-vector?
    (loop for i from 0 below (length new-data) by 4 do
	 (incf (aref new-data i) x-offset)
	 (incf (aref new-data (1+ i)) y-offset))
    ;; move lisp *verts* data to gl-array: *vertex-positions*
    (setf new-data (arc:create-gl-array-from-vector new-data))
    (gl:bind-buffer :array-buffer position-buffer-object)
    (gl:buffer-sub-data :array-buffer new-data)
    (gl:bind-buffer :array-buffer 0)
    ))
       


(defparameter time-uniform 0.0)

(defun init-shader-program ()
  (let ((shader-list (list)))
    ;;oh c'mon how to make it local
    (push
     (arc:create-shader
      :vertex-shader
      (arc:file-to-string
       (merge-pathnames "vs-calc-offset.glsl" *glsl-directory*)))
     shader-list)
    (push
     (arc:create-shader
      :fragment-shader
      (arc:file-to-string
       (merge-pathnames "fs-frag-color.glsl" *glsl-directory* )))
     shader-list)
    (let ((program (arc:create-program-and-return-it shader-list))
	  (loop-duration-uniform)
	  (frag-loop-duration-uniform))
      ;; here be uniform locations handlels
      (setf time-uniform (gl:get-uniform-location program "time"))
      (setf loop-duration-uniform (gl:get-uniform-location program "loop_duration"))
      (setf frag-loop-duration-uniform
	    (gl:get-uniform-location program "frag_loop_duration"))
      (%gl:use-program program)
      (%gl:uniform-1f loop-duration-uniform 5.0)
      (%gl:uniform-1f frag-loop-duration-uniform 10.0)
      ;;(gl:use-program 0)
      )
    (loop for shader-object in shader-list
       do (%gl:delete-shader shader-object))))


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
  (%gl:uniform-1f time-uniform (/ (sdl2:get-ticks) 1000.0))
  (%gl:draw-arrays :triangles 0 3)
  )

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


