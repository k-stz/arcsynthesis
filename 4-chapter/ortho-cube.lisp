(in-package #:arc-4)

(defvar *glsl-directory*
  (merge-pathnames #p "4-chapter/" (asdf/system:system-source-directory :arcsynthesis)))
;;TODO: what with this garbage here >_>, or should I really build the habit of looking
;; at the terminal
(defvar out *standard-output*)  (defvar dbg *debug-io*) (defvar err *error-output*)

(defparameter *vertex-positions* nil)

;;; TODODODODOODOD WHY COLORS NOT LIKE IN ARCSYNTHESIS EXAMPLE???
;; >_> elegant succinct code
;; the first 36 values are vertex the next 36 colors
  (defparameter *verts* #(
;; vertices:

	 0.25  0.25 0.75  1.0
	 0.25 -0.25 0.75  1.0
	-0.25  0.25 0.75  1.0

	 0.25 -0.25 0.75  1.0
	-0.25 -0.25 0.75  1.0
	-0.25  0.25 0.75  1.0

	 0.25  0.25 -0.75 1.0
	-0.25  0.25 -0.75 1.0
	 0.25 -0.25 -0.75 1.0

       	 0.25 -0.25 -0.75 1.0
       	-0.25  0.25 -0.75 1.0
       	-0.25 -0.25 -0.75 1.0

       	-0.25  0.25  0.75  1.0
       	-0.25 -0.25  0.75  1.0
       	-0.25 -0.25 -0.75  1.0

       	-0.25  0.25  0.75 1.0
       	-0.25 -0.25 -0.75 1.0
       	-0.25  0.25 -0.75 1.0

	 0.25  0.25  0.75 1.0
	 0.25 -0.25 -0.75 1.0
	 0.25 -0.25  0.75 1.0

	 0.25  0.25  0.75 1.0
	 0.25  0.25 -0.75 1.0
	 0.25 -0.25 -0.75 1.0

	 0.25 0.25 -0.75  1.0
	 0.25 0.25  0.75  1.0
	-0.25 0.25  0.75  1.0

	 0.25 0.25 -0.75  1.0
	-0.25 0.25  0.75  1.0
	-0.25 0.25 -0.75  1.0

	 0.25 -0.25 -0.75 1.0
	-0.25 -0.25  0.75 1.0
	 0.25 -0.25  0.75 1.0

	 0.25 -0.25 -0.75 1.0
	-0.25 -0.25 -0.75 1.0
	-0.25 -0.25  0.75 1.0

;; colors:

	0.0 0.0 1.0 1.0
      	0.0 0.0 1.0 1.0
	0.0 0.0 1.0 1.0

	0.0 0.0 1.0 1.0
	0.0 0.0 1.0 1.0
	0.0 0.0 1.0 1.0

	0.8 0.8 0.8 1.0
	0.8 0.8 0.8 1.0
	0.8 0.8 0.8 1.0

	0.8 0.8 0.8 1.0
	0.8 0.8 0.8 1.0
	0.8 0.8 0.8 1.0

	0.0 1.0 0.0 1.0
	0.0 1.0 0.0 1.0
	0.0 1.0 0.0 1.0

	0.0 1.0 0.0 1.0
	0.0 1.0 0.0 1.0
	0.0 1.0 0.0 1.0

	0.5 0.5 0.0 1.0
	0.5 0.5 0.0 1.0
	0.5 0.5 0.0 1.0

	0.5 0.5 0.0 1.0
	0.5 0.5 0.0 1.0
	0.5 0.5 0.0 1.0

	1.0 0.0 0.0 1.0
	1.0 0.0 0.0 1.0
	1.0 0.0 0.0 1.0

	1.0 0.0 0.0 1.0
	1.0 0.0 0.0 1.0
	1.0 0.0 0.0 1.0

	0.0 1.0 1.0 1.0
	0.0 1.0 1.0 1.0
	0.0 1.0 1.0 1.0

	0.0 1.0 1.0 1.0
	0.0 1.0 1.0 1.0
   	0.0 1.0 1.0 1.0	))


(setf *vertex-positions* (arc::create-gl-array-from-vector *verts*))

(defparameter position-buffer-object nil) ; buffer object handle
(defparameter x-offset 0) (defparameter y-offset 0)
(defparameter program nil "program-object")
(defparameter offset-uniform 0)

(defun init-shader-program ()
  (let ((shader-list (list)))
    ;;oh c'mon how to make it local
    (push (arc:create-shader
	   :vertex-shader
	   (arc:file-to-string (merge-pathnames "vs.glsl" *glsl-directory*)))
	  shader-list)
    (push (arc:create-shader
    	   :fragment-shader
    	   (arc:file-to-string (merge-pathnames "fs.glsl" *glsl-directory* )))
    	  shader-list)
    (setf program (arc:create-program-and-return-it shader-list))
    (setf offset-uniform (gl:get-uniform-location program "offset"))
    (%gl:use-program program)
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
  ;; duh, we need to multiply with 4 as every *verts* component is 4 bytes long
  (%gl:vertex-attrib-pointer 1 4 :float :false 0 (/ (* 4 (length *verts*)) 2))

  ;; new hot code:
  ;;enables "face culling" i.e. certain triangles won't be rendered -- performace boostaaa
  (gl:enable :cull-face)
  ;; the "back" side of a triangles won't be rendered
  ;; NOTE: :front-and-back to cull them all (e.g. for shader performance tests, as nothing gets
  ;; drawn to the string)
  (%gl:cull-face :back)
  ;; super interesting: this one decides the "back" and "front" sides of a triangle,
  ;; if :cw is set -- clock-wise -- we take three vertices in the order in which  they appear
  ;; (this order is called the WINDING ORDER)
  ;; in our :array-buffer and we (the viewer) look at them as they get drawn on the screen
  ;; if they appear in clock-wise distribution order this is considered the front side of
  ;; triangle (opposite: :ccw -- counter-clock-wise)
  (%gl:front-face :cw) ; help gl:cull-face decide who is a :back triangle
  ;; for this to work our *verts* must have vertices distributed accordingly so that those
  ;; facing back are classified as :back triangles
  ;; all it takes is the faces on the outside of the prism to have this property, so that
  ;; if we where to look through the prism, the outward :cw triangel would be back-to-front
  ;; -> always culled np! AAAH that's why "clipping" in games causes walls to disappear entirely
  ;; from view when you traverse them and look at them from behind!!!
  )


(defun rendering-code ()
  (gl:clear :color-buffer-bit)
  (%gl:uniform-2f offset-uniform 0.5 0.25)
  ;; can't see the sides of the rectangular prism!!! AWESOME
  (%gl:draw-arrays :triangles 0 36)
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
