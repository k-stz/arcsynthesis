(in-package #:arc-1)

;;;; cl version of arcsynthesis' "Chapter 1.Hello, Triangle!"

(defvar *sdl-init* nil) ;what was the use of that...?
(defvar out *standard-output*)  (defvar dbg *debug-io*) (defvar err *error-output*)
;(defparameter *connection* (or swank::*emacs-connection* (swank::default-connection)))

;;gl:with-gl-array might remedy this!
;; const float vertex-positions[] } { 0.75f, ... 1.0f, };
;;if *vectex-positions* isn't of type gl:gl-array (which it gets from gl:alloc-gl-array)
;;(gl:buffer-data .. *vector-positions*) will complain explicitly about this, and sdl2 will crash
(defparameter *vertex-positions* (gl:alloc-gl-array :float 12)) ;TODO: lisp builtins possible?
;;the alpha value seems to cause some scaling to happen...?
(defparameter *verts* #(0.75  0.75 0.0 1.0
			0.75 -0.75 0.0 1.0
			-0.75 -0.75 0.0 1.0
                         0.6 0.75 0.0 1.0
			 -0.75 0.75 0.0 1.0
			 -0.75 -0.6 0.0 1.0)) ;very strange: enter negative values for alpha... wtf
(defparameter *vertex-positions* (arc::create-gl-array-from-vector *verts*))

;;shader functions:
(defparameter *vertex-shader* "
#version 330  //coments desu!!!
/* these too?? */
layout(location = 0) in vec4 position;
void main () {
    gl_Position = position;
}")

(defparameter *fragment-shader* "
#version 330
out vec4 outputColor;
void main() {
outputColor = vec4(0.0f, 1.0f, 1.0f, 1.0f);
}")


(defun initialize-program ()
  (let ((shader-list (list)))
    (push (arc:create-shader :vertex-shader *vertex-shader*) shader-list)
    (push (arc:create-shader :fragment-shader *fragment-shader*) shader-list)
    (arc:create-program shader-list)
    ;; got to delete the shader objects too apparently >_>
    ;; maybe because they're residue as in the program object is already created from
    ;; their construction plan
     (loop for shader-object in shader-list
	  do (%gl:delete-shader shader-object))
  ))


(defun main ()
  (sdl2:with-init (:everything) ; k-stz: sdl2:in-main-thread code!!
    (progn (setf *standard-output* out) (setf *debug-io* dbg) (setf *error-output* err))
    ;K-STZ TEST:
    (format t "Using SDL Library Version: ~D.~D.~D~%"
            sdl2-ffi:+sdl-major-version+
            sdl2-ffi:+sdl-minor-version+
            sdl2-ffi:+sdl-patchlevel+)
    ;;SDL_Window *window = SDL_CreateWindow(
    ;; "SDL2 WINDOW", 0, 0, 640, 480, 
    ;; SDL_WINDOW_OPENGL|SDL_WINDOW_RESIZABLE); ; i think all flaggs are used by default
    (sdl2:with-window (win :w 200 :h 200 :flags '(:shown :opengl)) ;these flags necessary?
      ;; SDL_GLContext glcontext = SDL_GL_CreateContext(window)
      ;; also once its <body> is executed it runs:
      ;; SDL_GL_DeleteContext(glcontext)  CL: (sdl2:gl-delete-context gl-context)
      (sdl2:with-gl-context (gl-context win)
	;;straight arcsynthesis code:
	(%gl:clear-color 0 0 1 1)	;glClearColor(0,0,0,0)   ;0.0f ? float it too?
	(gl:clear :color-buffer-bit)	;glClear(GL_COLOR_BUFFER_BIT)
	;;create buffer object
	(let ((position-buffer-object (first (gl:gen-buffers 1)))) ;glGenBuffers(1, &positionBufferObject);
	  (%gl:bind-buffer :array-buffer position-buffer-object)
	  ;;sharp student notes: position-buffer-object receives a COPY of the contents of vertex-positions in the following line, "implicitly"!
;;;;DEVIATION:
	  (gl:buffer-data :array-buffer :static-draw *vertex-positions*) ;glBufferData(GL_ARRAY_BUFFER, sizeof(vertexPositions), vertexPositions, GL_STATIC_DRAW)
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
	       ;;; hm, doesn't work, but lets not pursue this, since their is probably
	       ;;; a canonical way to do this anyway! (static-draw is my foe?)
               ;; (%gl:bind-buffer :array-buffer 0)
	       ;; (gl:buffer-data :array-buffer :static-draw
	       ;; 		       (create-gl-array-from-vector #(1.0 1.0 1.0 0.0)))
	       ;; (print 'WINITUDE)
	       ;(%gl:viewport 0 0 200 200)
	       ;; clears screen with color set in glClearColor!!
	       (gl:clear :color-buffer-bit) ;glClear(GL_COLOR_BUFFER_BIT)
	       )

	     (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
	       (sdl2:push-event :quit))
	     (let ((k (sdl2:scancode-value keysym)))
	       (format t "~a~%" k)))
	    (:idle () ;;;main rendering code:
                  ; (arc::update-swank) ;TODO: new slime-connect for this??
		  ; (swank::handle-requests *connection* t) ;TODO, do NOT understand
		  ; (swank::handle-requests (swank::default-connection) t)
		   ;; TODO: state setting functions don't need to run every iteration
		   ;; of the rendering loop, instead only when change is needed!!
		   (gl:bind-buffer :array-buffer position-buffer-object)
		   ;;both /attrib/ functions take an attribute index as their first parameter
		   (%gl:enable-vertex-attrib-array 0) 
		   (%gl:vertex-attrib-pointer 0 4 :float :false 0 0)

		   ;;now OpenGL knows where to get its vertex data; rendering 2 triangel (6 vertexes)
                   (%gl:draw-arrays :triangles 0 6)
		   (sdl2:gl-swap-window win) ; this should have been implicit
		   )
	    (:quit () t)))))))

;;changing this function effectively allows live-editing, why then
;;can't I live edit
(defun foo (var)
  (print var))
