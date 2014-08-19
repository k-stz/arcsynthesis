(in-package #:arc-3)

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

(defparameter *curr-sdl-ticks* 0)
(defparameter *frame-counter* 0)

;;TODO: into auxiliary-functions.lisp ? probably problems due to use of (sdl2:get-ticks)
(defun framelimit (fps)
  (let* ((time-passed (- (sdl2:get-ticks) *curr-sdl-ticks*))
	 (should-pass (/ 1000.0 fps))
	 (wait-time (round (- should-pass time-passed ))))
;    (format t "time-passed:~a should-pass:~a wait-time:~a~%" time-passed should-pass wait-time)
    (when (> wait-time 0)
      (sdl2:delay wait-time)))
  (setf *curr-sdl-ticks* (sdl2:get-ticks)))
  

(defun compute-positions-offset () ;wow, we don't even need x-offset, y-offset (lol!)
  "Return a list containing values which oscilate every 5 seconds"
  (let* ((loop-duration 5.0)
	 (scale (/ (* pi 2) loop-duration))
	 (elapsed-time (/ (sdl2:get-ticks) 1000.0))
	 ;; this is sorta cool, it in effect creates a value on the range [0, loop-duration)
	 ;; which is what we want: representing the 5 seconds circle!
	 (curr-time-through-loop (mod elapsed-time loop-duration)))
    ;; in the following "0.5" shrinks the cos/sin oscilation from 1 to -1 to 0.5 to -0.5
    ;; in effect creating a "circle of diameter 1 (from 0.5 to -0.5 = diameter 1)
    (setf x-offset (* (cos (* curr-time-through-loop scale)) 0.5))
    (setf y-offset (* (sin (* curr-time-through-loop scale)) 0.5))))

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
    ;; (%gl:buffer-sub-data target offset size data)
    ;; we use this instead of gl:buffer-data, because the memory was already allocated by
    ;; gl:buffer-data. gl:buffer-SUB-data operates on existing memory ;>
    ;; TODO: arcsynthesis code uses a fresh gl:gl-array instead of reusing (changing)
    ;; *vertex-positions* ... :( YEAH this is wrong, we add shit to its parameters
    ;; all the time, hence our triangle spins out of existence yo!!!
    (gl:buffer-sub-data :array-buffer new-data)
    (gl:bind-buffer :array-buffer 0)
    ))
       


(defun init-shader-program ()
  (let ((shader-list (list)))
    ;;oh c'mon how to make it local
    (push
     (arc:create-shader
      :vertex-shader
      (arc:file-to-string
       (merge-pathnames "vertex-shader-3.glsl" *glsl-directory*)))
     shader-list)
    (push
     (arc:create-shader
      :fragment-shader
      (arc:file-to-string
       (merge-pathnames "fragment-shader-3.glsl" *glsl-directory* )))
     shader-list)
    ;; TODO:fragment shader
    (arc:create-program shader-list)
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
  (%gl:enable-vertex-attrib-array 0) ; vertex array
  (%gl:enable-vertex-attrib-array 1) ; color array
  (%gl:vertex-attrib-pointer 0 4 :float :false 0 0)
  ;; this works :I, so does simply '48' as well :x
  (%gl:vertex-attrib-pointer 1 4 :float :false 0 (cffi:make-pointer 48)) 
  )


(defun rendering-code ()
  ;;strange arcsynthesis repeadetly calls "glUseProgram" hmm
  (gl:clear :color-buffer-bit)
  (compute-positions-offset)
  (adjust-vertex-data)
  (%gl:draw-arrays :triangles 0 3)
  )

(defun main ()
  (sdl2:with-init (:everything)
    (progn (setf *standard-output* out) (setf *debug-io* dbg) (setf *error-output* err))
    (setf *curr-sdl-ticks* (sdl2:get-ticks)) ;for framelimit calculation
    (sdl2:with-window (win :w 400 :h 400 :flags '(:shown :opengl))
      (sdl2:with-gl-context (gl-context win)
	(gl:clear-color 0 0 0.2 1)
	(gl:clear :color-buffer-bit)
        (set-up-opengl-state)
	(init-shader-program)	    ; implicitly gl:use-program :I
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
		 ;;TODO: hm if tearing occurs try (gl:flush) (gl:finish) ..
		 ;; it makes the whole animation jitter though
		 ; (gl:finish) hmmmm i bet its due to batshit crazy high framerate
		 ;; from lazyfoo tutorial:
		 ;; SDL_Delay( ( 1000 / FRAMES_PER_SECOND ) - fps.get_ticks() ) ;
                 ;; TODO write framelimiter using sdl2:get-ticks -.-" i need them fo
		 ;; WOW, still doesn't solve tearing problem, it is even possible that this problem
		 ;; occurs at a even higher level than I can modify in my application
                 (framelimit 60)
		 (sdl2:gl-swap-window win) ; wow, this can be forgotten easily -.-
		 ))))))


