(in-package #:arc-4.1)

(defvar *data-directory*
  (merge-pathnames #p "4-chapter/data/" (asdf/system:system-source-directory :arcsynthesis)))

(defvar out *standard-output*)  (defvar dbg *debug-io*) (defvar err *error-output*)

(defparameter *vertex-positions* nil)

;; OMG OMG OMG OMG: THE VERTEX DATA CHANGED FROM LAST EXAMPLE!!!!!!!!!!!!!!!!!!!!!!!!!
;; was trying to find the error for HOURS >_<
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

(defvar *position-buffer-object*) ; buffer object handle
(defvar *program*)
(defparameter *offset-uniform* 0)
(defvar *z-near-uniform*)
(defvar *z-far-uniform*)
(defvar *frustum-scale-uniform*)

(defun init-shader-program ()
  (let ((shader-list (list)))
    ;;oh c'mon how to make it local
    (push (arc:create-shader
	   :vertex-shader
	   (arc:file-to-string (merge-pathnames "manual-perspective.vert" *data-directory*)))
	  shader-list)
    (push (arc:create-shader
    	   :fragment-shader
    	   (arc:file-to-string (merge-pathnames "standard-colors.frag" *data-directory* )))
    	  shader-list)
    (setf *program* (arc:create-program shader-list))

    (setf *offset-uniform* (gl:get-uniform-location *program* "offset"))

    (setf *frustum-scale-uniform* (gl:get-uniform-location *program* "frustumScale"))
    (setf *z-near-uniform* (gl:get-uniform-location *program* "zNear"))
    (setf *z-far-uniform* (gl:get-uniform-location *program* "zFar"))

    (%gl:use-program *program*)
    (%gl:uniform-1f *frustum-scale-uniform* 1.0)
    (%gl:uniform-1f *z-near-uniform* 1.0)

    (%gl:uniform-1f *z-far-uniform* 3.0)
    
    (loop for shader-object in shader-list
       do (%gl:delete-shader shader-object))))


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
  (gl:viewport 0 0 500 500)
  )


(defun rendering-code ()
  (gl:clear :color-buffer-bit)
  (%gl:uniform-2f *offset-uniform* 0.5 0.5)
  (%gl:draw-arrays :triangles 0 36))

(defun main ()
  (sdl2:with-init (:everything)
    (progn (setf *standard-output* out) (setf *debug-io* dbg) (setf *error-output* err))
    (sdl2:with-window (win :w 500 :h 500 :flags '(:shown :opengl))
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
	     (gl:disable :cull-face))
	   (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
	     (sdl2:push-event :quit)))
	  (:quit () t)
	  (:idle ()
		 ;;main-loop:
		 (rendering-code)
		 (sdl2:gl-swap-window win) ; wow, this can be forgotten easily -.-
		 ))))))
