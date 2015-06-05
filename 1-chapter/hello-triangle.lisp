;;;; cl version of arcsynthesis' "Chapter 1.Hello, Triangle!"

(in-package #:arc-1)

(defvar out *standard-output*)  (defvar dbg *debug-io*) (defvar err *error-output*)

;;the alpha value seems to cause some scaling to happen...?
(defvar *verts* #(0.75  0.75 0.0 1.0
			0.75 -0.75 0.0 1.0
			-0.75 -0.75 0.0 1.0
                         0.6 0.75 0.0 1.0
			 -0.75 0.75 0.0 1.0
			 -0.75 -0.6 0.0 1.0)) ;very strange: enter negative values for alpha... wtf

;; if *vectex-positions* isn't of type gl:gl-array (which it gets from gl:alloc-gl-array)
;; (gl:buffer-data .. *vector-positions*) will complain explicitly about this, and sdl2
;; will crash
(defvar *vertex-positions* (arc:create-gl-array-from-vector *verts*))

;; alternatively
;; (cffi:foreign-alloc :float :initial-contents '(0.75  0.75 0.0 1.0
;; 						       0.75 -0.75 0.0 1.0
;; 						       -0.75 -0.75 0.0 1.0
;; 						       0.6 0.75 0.0 1.0
;; 						       -0.75 0.75 0.0 1.0
;; 						       -0.75 -0.6 0.0 1.0))
;; can be used, the gl:gl-array is just a cl-struct with the above forein memory pointer
;; plus size and type information attached to it. Further benefit is that most of the
;; "gl:" wrapper functions expect data in the form of a gl-array. Such as (gl:buffer-data
;; ..)  while the actuall bindings (%gl:buffer-data ...) (note the "%" sign indicating the
;; direct gl API binings) can take the direct forein data as provided by the above
;; (cffi:foreing-alloc.. ). If the size information is needed, that's where gl-array comes
;; in handy, being struct with the field "size" so we can simply (gl::gl-array-size
;; <gl-array>) and get it.
;; Note "size" stores the number of elements of a certain :type that is stored within, If
;; the byte size is needed you need to get the (cffi:foreign-type-size (gl::gl-array-type
;; <gl-array>)) and finally multiply it by the (gl::gl-array-size <gl-array>) above.
;; Anyway, that's pertaining dealing with the API at the lowlevel, there are already lispy
;; solutions available to this, such as in the code for Chris Bagley's CEPL - "code eval
;; play loop" (LLGPL'd).

;;shader functions:
(defvar *vertex-shader* "
#version 330  //coments desu!!!
/* these too?? */
layout(location = 0) in vec4 position;
void main () {
    gl_Position = position;
}")

(defvar *fragment-shader* "
#version 330
out vec4 outputColor;
void main() {
outputColor = vec4(0.0f, 1.0f, 1.0f, 1.0f);
}")


(defun initialize-program ()
  (let ((shader-list (list)))
    (push (arc:create-shader :vertex-shader *vertex-shader*) shader-list)
    (push (arc:create-shader :fragment-shader *fragment-shader*) shader-list)
    (arc:create-program shader-list)))


(defun main ()
  (sdl2:with-init (:everything)
    (progn (setf *standard-output* out) (setf *debug-io* dbg) (setf *error-output* err))
    (format t "Using SDL Library Version: ~D.~D.~D~%"
            sdl2-ffi:+sdl-major-version+
            sdl2-ffi:+sdl-minor-version+
            sdl2-ffi:+sdl-patchlevel+)
    ;;SDL_Window *window = SDL_CreateWindow(
    ;; "SDL2 WINDOW", 0, 0, 640, 480, 
    ;; SDL_WINDOW_OPENGL|SDL_WINDOW_RESIZABLE); ; :resizeable can be used here
    (sdl2:with-window (win :w 200 :h 200 :flags '(:shown :opengl)) ;these flags necessary?
      ;; SDL_GLContext glcontext = SDL_GL_CreateContext(window)
      ;; also once its <body> is executed it runs:
      ;; SDL_GL_DeleteContext(glcontext)  CL: (sdl2:gl-delete-context gl-context)
      (sdl2:with-gl-context (gl-context win)
	;;straight arcsynthesis code:
	(%gl:clear-color 0 0 1 1)	;glClearColor(0,0,0,0)
	(gl:clear :color-buffer-bit)	;glClear(GL_COLOR_BUFFER_BIT)
	;;create buffer object
	(let ((position-buffer-object (first (gl:gen-buffers 1)))) ;glGenBuffers(1, &positionBufferObject);
	  (%gl:bind-buffer :array-buffer position-buffer-object)
	  ;;sharp student notes: position-buffer-object receives a COPY of the contents of
	  ;;vertex-positions in the following line, "implicitly"!

	  ;;Minor DEVIATIONS:---------------------------------------------------
	  ;;glBufferData(GL_ARRAY_BUFFER, sizeof(vertexPositions), vertexPositions, GL_STATIC_DRAW)
	  (gl:buffer-data :array-buffer :static-draw *vertex-positions*)
	  (gl:bind-buffer :array-buffer 0)
          ;;init shaders:
	  (initialize-program)
	  
	  ;;MAIN-LOOP
	  (sdl2:with-event-loop (:method :poll)
	    (:keyup
	     (:keysym keysym)
	     (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-r)
	       (format t "resizing!~%") (sdl2:set-window-size win 200 200))
	     (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-e)
	       ;;experimental code
	       (print "key-pressed: e -- executing experiments...:~%")
	       ;; clears screen with color set in glClearColor!!
	       (gl:clear :color-buffer-bit) ;glClear(GL_COLOR_BUFFER_BIT)
	       )

	     (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
	       (sdl2:push-event :quit))
	     (let ((k (sdl2:scancode-value keysym)))
	       (format t "~a~%" k)))
	    (:idle () 
		   ;; TODO: state setting functions don't need to run every iteration
		   ;; of the rendering loop, instead only when change is needed!!
		   (gl:bind-buffer :array-buffer position-buffer-object)
		   ;;both /attrib/ functions take an attribute index as their first parameter
		   (%gl:enable-vertex-attrib-array 0) 
		   (%gl:vertex-attrib-pointer 0 4 :float :false 0 0)

		   ;;now OpenGL knows where to get its vertex data; rendering 2 triangles (6 vertexes)
                   (%gl:draw-arrays :triangles 0 6)
		   (sdl2:gl-swap-window win) ; this should have been implicit
		   )
	    (:quit () t)))))))
