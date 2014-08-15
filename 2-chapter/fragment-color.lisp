(in-package #:arc-2)

(defvar *glsl-directory*
  (merge-pathnames #p "2-chapter/" (asdf/system:system-source-directory :arcsynthesis)))
(defvar *vertex-shader.glsl-path*
  (merge-pathnames #p "vertex-shader.glsl" *glsl-directory*))

;;; this time we load shaders from files, check out the (init-shader-program) function

(defvar *vertex-positions* (gl:alloc-gl-array :float 12))
(defparameter *verts* #(0.75  0.75 0.0 1.0
			0.75 -0.75 0.0 1.0
			-0.75 -0.75 0.0 1.0
                         0.6 0.75 0.0 1.0
			 -0.75 1.0 0.0 1.0
			 -0.75 0.0 0.0 1.0))
(setf *vertex-positions* (arc::create-gl-array-from-vector *verts*))


(defun init-shader-program ()
  (let ((shader-list (list)))
    (push (arc:create-shader
	   :vertex-shader
	   (arc:file-to-string (merge-pathnames "vertex-shader.glsl" *glsl-directory*)))
	  shader-list)
    (push (arc:create-shader
	   :fragment-shader
	   (arc:file-to-string (merge-pathnames "fragment-shader.glsl" *glsl-directory*)))
	  shader-list)
    ;; TODO:fragment shader
    (arc:create-program shader-list)
    (loop for shader-object in shader-list
	  do (%gl:delete-shader shader-object))))

(defparameter position-buffer-object nil) ;; buffer object handle

(defun set-up-opengl-state ()
  (setf position-buffer-object (first (gl:gen-buffers 1)))
  (%gl:bind-buffer :array-buffer position-buffer-object)
  ;; TODO: don't use arc-1::*vertex-positions*
  (gl:buffer-data :array-buffer :static-draw *vertex-positions*)
  (gl:bind-buffer :array-buffer 0)
  (gl:bind-buffer :array-buffer position-buffer-object)
  (%gl:enable-vertex-attrib-array 0) 
  (%gl:vertex-attrib-pointer 0 4 :float :false 0 0)
  )


(defun rendering-code ()
  (%gl:draw-arrays :triangles 0 6)
  )

;;macro abstracting all the basic stuff from 1-chapter one away:
(defun main ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :w 400 :h 400 :flags '(:shown :opengl))
      (sdl2:with-gl-context (gl-context win)
	;;glClearColor(..)
	(gl:clear-color 0 0 0.2 1)
	(gl:clear :color-buffer-bit)
	;;TODO: array-buffer code:
        (set-up-opengl-state)
	(init-shader-program) ;implicitly gl:use-program :I
	(sdl2:with-event-loop (:method :poll)
	  (:keyup
	   (:keysym keysym)
	   (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-e)

	     )
	   (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
	       (sdl2:push-event :quit)))
	  (:quit () t)
	  (:idle ()
		 ;;main-loop:
		 (rendering-code)
		 (sdl2:gl-swap-window win) ; wow, this can be forgotten easily -.-
		 ))))))



;; just for fun, this ONLY WORKS FOR LINEAR INTERPOLATION ... NOT LINEAR _EXTRAPOLATION_
(defun linear-interpolation (x0 y0 x1 y1 x)
  (let* ((rec-1 (* y0 (abs (- x x1))))
	 (rec-2 (* y1 (abs (- x x0))))
	 (rec-sum (+ rec-1 rec-2)))
    (float (/ rec-sum (abs (- x1 x0))))))
	

(defun lerp (x0 y0 x1 y1 x)
  "linear inter/extrapolation calculation of 'y'"
  (+ y0
     (/ (* (- y1 y0) (- x x0))
	(- x1 x0))))

;; groovy:
;; (format t "~A~%" #\black_hexagon)
;; (format t"~a" #\GREEK_SMALL_LETTER_LAMDA) ;;note the lamda lacking its b:

