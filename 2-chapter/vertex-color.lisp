(in-package #:arc-2.1)
;;TODO: ugly solution fix this yo
;;yeah this code will still set the "global" variable for all packages so to speak:
;;TODO use (asdf/system:system-source-directory ...) + merge-pathname
(setf *default-pathname-defaults* #P"/home/k-stz/sol_sanctum/common_lisp/arcsynthesis/2-chapter/")
;;TODO: what with this garbage here >_>, or should I really build the habit of looking
;; at the terminal
(defvar out *standard-output*)  (defvar dbg *debug-io*) (defvar err *error-output*)



;;; this time we load shaders from files, check out the (init-shader-program) function

(defvar *vertex-positions* (gl:alloc-gl-array :float 12))
;;vertecies followed by colors, also this time an equilateral triangle instead of a isosceles
(defparameter *verts* #(0.0  0.5   0.0 1.0
			0.5 -0.366 0.0 1.0
		       -0.5 -0.366 0.0 1.0
			1.0  0.0   0.0 1.0
			0.0  1.0   0.0 1.0
			0.0  0.0   1.0 1.0)) 

(setf *vertex-positions* (arc::create-gl-array-from-vector *verts*))

(defvar *program*)

(defun init-shader-program ()
  (let ((shader-list (list)))
    ;;oh c'mon how to make it local
    (push
     (arc:create-shader :vertex-shader
			(arc:file-to-string "multi-input-vertex-shader.glsl"))
     shader-list)
    (push
     (arc:create-shader :fragment-shader
			(arc:file-to-string "fragment-shader-with-input.glsl"))
     shader-list)
    (setf *program* 
	  (arc:create-program shader-list))
    (loop for shader-object in shader-list
       do (%gl:delete-shader shader-object))))

(defparameter position-buffer-object nil) ;; buffer object handle

(defun set-up-opengl-state ()
  (setf position-buffer-object (first (gl:gen-buffers 1)))
  (%gl:bind-buffer :array-buffer position-buffer-object)

  (gl:buffer-data :array-buffer :static-draw *vertex-positions*)
  (gl:bind-buffer :array-buffer 0)
  (gl:bind-buffer :array-buffer position-buffer-object)
  (%gl:enable-vertex-attrib-array 0) ; vertex array
  (%gl:enable-vertex-attrib-array 1) ; color array
  (%gl:vertex-attrib-pointer 0 4 :float :false 0 0)
  ;; TODO: very important, look up implementation of this function, as to how the
  ;; (void*)48 could be omitted!
  (%gl:vertex-attrib-pointer 1 4 :float :false 0 (cffi:make-pointer 48)) ;this works :I, so does simply '48' as well :x
;;   (cffi-sys:with-foreign-pointer (ptr 1)
;;     (setf (cffi:mem-ref ptr :int) 48)
;;    (%gl:vertex-attrib-pointer 1 4 :float :false 0 ptr))
  
;;  (%gl:disable-vertex-attrib-array 0)
;;  (%gl:disable-vertex-attrib-array 1)

  )


(defun rendering-code ()
  (gl:use-program *program*)
  (%gl:draw-arrays :triangles 0 3))

(defun main ()
  (sdl2:with-init (:everything)
    (progn (setf *standard-output* out) (setf *debug-io* dbg) (setf *error-output* err))

    (sdl2:with-window (win :w 400 :h 400 :flags '(:shown :opengl))
      (sdl2:with-gl-context (gl-context win)
	;;glClearColor(..)
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
