;; TODO: about

(in-package #:arc-8.3)

;; TODO: this might solve the problem:
;; (print (uiop/lisp-build:current-lisp-file-pathname)) ?
(defvar *glsl-directory*
  (merge-pathnames #p "8-chapter/" (asdf/system:system-source-directory :arcsynthesis)))
 (defvar *data-dir*
   (merge-pathnames #p "data/" *glsl-directory*))

;;todo: fix this output to slime-repl solution
(defvar out *standard-output*)  (defvar dbg *debug-io*) (defvar err *error-output*)

(defvar *program*)
(defvar *model-to-camera-matrix-unif*)
(defvar *camera-to-clip-matrix-unif*)
(defvar *base-color-unif*)

(defparameter *camera-to-clip-matrix* (glm:make-mat4 0.0))
(defparameter *frustum-scale* (glutil::calc-frustum-scale 20.0)) 

(defun initialize-program ()
  (let ((shader-list (list)))
    ;;oh c'mon how to make it local
    (push (arc:create-shader
	   :vertex-shader
	   (arc:file-to-string
	    (merge-pathnames "pos-color-local-transformation.vert" *glsl-directory*)))
	  shader-list)
    (push (arc:create-shader
    	   :fragment-shader
    	   (arc:file-to-string
	    (merge-pathnames "color-mult-uniform.frag" *glsl-directory* )))
    	  shader-list)
    (setf *program* (arc:create-program-and-return-it shader-list)))

  (setf *model-to-camera-matrix-unif*
	(gl:get-uniform-location *program* "model_to_camera_matrix"))
  (setf *camera-to-clip-matrix-unif*
	(gl:get-uniform-location *program* "camera_to_clip_matrix"))
  (setf *base-color-unif*
	(gl:get-uniform-location *program* "base_color"))

  (format t "a:~a b:~a~%" *model-to-camera-matrix-unif* *camera-to-clip-matrix-unif*)

  (let ((fz-near 1.0)
	(fz-far 600.0))
    (glm:set-mat4 *camera-to-clip-matrix* 0 :x *frustum-scale*)
    (glm:set-mat4 *camera-to-clip-matrix* 1 :y *frustum-scale*)
    (glm:set-mat4 *camera-to-clip-matrix* 2 :z (/ (+ fz-far fz-near)
						  (- fz-near fz-far)))
    (glm:set-mat4 *camera-to-clip-matrix* 2 :w -1.0)
    (glm:set-mat4 *camera-to-clip-matrix* 3 :z (/ (* 2 fz-far fz-near)
						  (- fz-near fz-far)))

    (%gl:use-program *program*)
    (gl:uniform-matrix *camera-to-clip-matrix-unif*  4 (vector *camera-to-clip-matrix*)
		       NIL))
  (%gl:use-program 0))


(defvar *ship-vao*)
(defvar *plane*)

(defun init ()
  (initialize-program)

  (setf *ship-vao* (framework:ship-xml->vao (merge-pathnames *data-dir* "Ship.xml")))
  (setf *plane* (framework:xml->mesh-obj (merge-pathnames *data-dir* "UnitPlane.xml")))

  
  (gl:enable :cull-face)
  (%gl:cull-face :back)
  (%gl:front-face :cw) 

  (gl:viewport 0 0 500 500)

  (gl:enable :depth-test)
  (gl:depth-mask :true)
  (%gl:depth-func :lequal)
  (gl:depth-range 0.0 1.0)
  )



(defparameter *sphere-cam-rel-pos* (glm:vec3 90.0 0.0 66.0) "Order: phi theta r")
(defparameter *cam-target* (glm:vec3 0.0 10.0 0.0))

(defun resolve-cam-position ()
  "Spherical coordinates stored in *sphere-cam-rel-pos* are transformed to Euclidean
geometry coordinates and returned as a position vector."
  (let* ((phi (framework:deg-to-rad (glm:vec. *sphere-cam-rel-pos* :x)))
	 (theta (framework:deg-to-rad (+ (glm:vec. *sphere-cam-rel-pos* :y)
					 90.0)))
	 (sin-theta (sin theta))
	 (cos-theta (cos theta))
	 (cos-phi (cos phi))
	 (sin-phi (sin phi))
	 ;;explanation in "world-with-ubo.lisp" in the chapter 7 directory
	 (dir-to-camera (glm:vec3 (* sin-theta cos-phi)
	 			  cos-theta
	 			  (* sin-theta sin-phi))))
    ;; Once the direction is set, we need to blow it up by multiplying the
    ;; "euclidean" vector by 'r' which is the :z value of *sphere-cam-rel-pos*
    ;; and add it to the target the camera is supposed to look at, thereby creating
    ;; a geometrical dependent positions of the camera to the *cam-target*
    (sb-cga:vec+ (sb-cga:vec* dir-to-camera (glm:vec. *sphere-cam-rel-pos* :z))
		 *cam-target*)))

(defun calc-look-at-matrix (camera-pt look-pt up-pt)
  ;; resolve-cam-position. negating the two yields the direction of the camera "through"
  ;; look-pt. Unit vector, through normalize, is needed as we will use look-dir
  ;; directly as the axis of the look-at-matrix, so as to prevent scaling!
  ;; With this we already have our Z axis
  (let* ((look-dir (sb-cga:normalize (sb-cga:vec- look-pt camera-pt)))
	 ;; since we only want a direction, we normalize the vector
	 ;; using up-dir we have a plane from which we can yield the perpendicular x-axis
	 ;; and from that the perpendicular y-axis (cross-product z-axis x-axis)
	 (up-dir (sb-cga:normalize up-pt))
	 ;; cross-product returns the vector perpendicular to the plane formed
	 ;; by two vectors:
	 ;; (sb-cga:cross-product (glm:vec3 1.0 0.0 0.0) (glm:vec3 0.0 1.0 0.0))
	 ;; ==> #(0.0 0.0 1.0)	 
	 (right-dir (sb-cga:normalize (sb-cga:cross-product look-dir up-dir)))
	 (perp-up-dir (sb-cga:cross-product right-dir look-dir))

	 (rot-mat (glm:make-mat4 1.0))
	 (trans-mat (glm:make-mat4 1.0)))

    (glm:set-mat4-col rot-mat 0 (glm:vec4-from-vec3 right-dir 0.0))
    (glm:set-mat4-col rot-mat 1 (glm:vec4-from-vec3 perp-up-dir 0.0))
    ;; the look-dir must be negated, or it wouldn't be a rotation of the original
    ;; identity matrix. As it currently would be a transform that couldn't arise
    ;; from rotating the original coordinate system (one axis is moved without
    ;; moving another by _the same_ angle). This must be a problem that arises from
    ;; using the cross-product, which doesn't seem to return the perpendicular axis
    ;; needed (there are always two perpendicular direction vectors for a plane)
    (glm:set-mat4-col rot-mat 2 (glm:vec4-from-vec3 (glm:vec- look-dir) 0.0))

    ;; TODO: why transpose it eventually? Maybe because it is col-major and setting
    ;; column-wise and transpose is more efficient than just setting the rows
    ;; with discontiguous indices
    (setf rot-mat (sb-cga:transpose-matrix rot-mat))

    ;; oh, its just a translation matrix putting the camera-pt into origin! (and thereby
    ;; offsetting every position sent through this matrix by the camera pos
    (glm:set-mat4-col trans-mat 3 (glm:vec4-from-vec3 (glm:vec- camera-pt) 1.0))

    ;;return rotmat * transmat;
    (sb-cga:matrix* rot-mat trans-mat)))


(defparameter *orientation* (glm:quaternion 1.0 0.0 0.0 0.0))


(defun draw ()
  (let ((cam-pos (resolve-cam-position))
	(curr-matrix (make-instance 'glutil:matrix-stack)))

    (glutil:set-matrix curr-matrix
    		       (calc-look-at-matrix cam-pos *cam-target* (glm:vec3 0.0 1.0 0.0)))


    (gl:use-program *program*)

    (glutil:with-transform (curr-matrix)
	:scale 100.0 1.0 100.0

	(%gl:uniform-4f *base-color-unif* 0.2 0.5 0.2 1.0)

	(gl:uniform-matrix *model-to-camera-matrix-unif* 4
			   (vector (glutil:top-ms curr-matrix)) NIL)
	(framework:render *plane*))
    
    (glutil:with-transform (curr-matrix)
	:translate (glm:vec. *cam-target* :x)
	           (glm:vec. *cam-target* :y)
		   (glm:vec. *cam-target* :z)

	:apply-matrix (glm:mat4-cast *orientation*)
	:rotate-x -90.0

        (%gl:uniform-4f *base-color-unif* 1.0 1.0 1.0 1.0)
	(gl:uniform-matrix *model-to-camera-matrix-unif* 4
			   (vector (glutil:top-ms curr-matrix)) NIL)
	(framework:render-ship *ship-vao*))

    (gl:use-program 0)))

(defun display ()
  (gl:clear-color 0.0 0.0 0.2 1)
  (gl:clear-depth 1.0)
  (gl:clear :color-buffer-bit :depth-buffer-bit)

  (draw))

(defparameter *fz-near* 1.0)
(defparameter *fz-far* 1000.0)

(defun reshape (w h)
  (glm:set-mat4 *camera-to-clip-matrix* 0 :x
		(* *frustum-scale* (/ h w)))
  (glm:set-mat4 *camera-to-clip-matrix* 1 :y *frustum-scale*)

  (%gl:use-program *program*)
  (gl:uniform-matrix *camera-to-clip-matrix-unif* 4 (vector *camera-to-clip-matrix*)
		     NIL)
  (%gl:use-program 0)

  (%gl:viewport 0 0 w h))



(defconstant +standard-angle-increment+ 11.25)
(defconstant +small-angle-increment+ 9.0)

;; essential code for this chapter:
(defparameter *slerp-toggle* NIL)

(defparameter *alpha-timer* 0.0)

(defun alpha-update ()
  (setf *alpha-timer* (mod (/ (sdl2:get-ticks) 5000.0) 1.0)))

(defvar *orientations-plist*
  (list :q (glm:quaternion 0.7071 0.7071 0.0 0.0)
	:w (glm:quaternion 0.5 0.5 -0.5 0.5)
	:e (glm:quaternion -0.4895 -0.7892 -0.3700 -0.02514)
	:r (glm:quaternion 0.4895 0.7892 0.3700 0.02514)

	:t (glm:quaternion 0.3840 -0.1591 -0.7991 -0.4344)
	:y (glm:quaternion 0.5537 0.5208 0.6483 0.0410)
	:z (glm:quaternion 0.5537 0.5208 0.6483 0.0410)
	:u (glm:quaternion 0.0 0.0 1.0 0.0)))

(defun get-orient (indicator-qwertzy)
  (getf *orientations-plist* indicator-qwertzy))

(defparameter *source-orientation* (glm:quaternion 1.0 0.0 0.0 0.0))
(defparameter *destination-orientation* (glm:quaternion 1.0 0.0 0.0 0.0))

(defparameter *source-p* t)
(defun set-source-or-destination-orientation (key)
  "Reads values from *orientations-plist* using key. Sets *source-orientation* if
*source-p* is true, destination otherwise. Toggles *source-p* after setting to facilitate
sequential input."
  (let ((orient (get-orient key)))
    (if *source-p*
	(progn (setf *source-orientation* orient)
	       (setf *source-p* NIL))
	(progn (setf *destination-orientation* orient)
	       (setf *source-p* t)))))



(defun test-lerp ()
  (setf *orientation* 
	(glm:mix *source-orientation*
		 *destination-orientation* *alpha-timer*)))

(defun test-slerp ()
  (setf *orientation*
	(glm::slerp-simple *source-orientation*
			   *destination-orientation* *alpha-timer*)))

(defparameter *source-key* 'identity)
(defparameter *destination-key* 'identity)
(defun print-info (&optional key)
  (when key
    (if *source-p*
	(setf *source-key* key)
	(setf *destination-key* key)))
  (format t "~a: ~a -> ~a~%"
	  (if *slerp-toggle* "SLERP" "LERP ")
	  *source-key*
	  *destination-key*))

(defun main ()
  (arc::with-main
    (sdl2:with-init (:everything)
      (progn (setf *standard-output* out) (setf *debug-io* dbg) (setf *error-output* err))
      (sdl2:with-window (win :w 500 :h 500 :flags '(:shown :opengl))
	(sdl2:with-gl-context (gl-context win)
	  ;; INIT code:
	  (init)
	  ;; TODO: callback for reshape; for now used to setup cam-to-clip-space matrix
	  (reshape 500.0 500.0)
	  (sdl2:with-event-loop (:method :poll)
	    (:keydown
	     (:keysym keysym)
	     ;; TODO: capture in macro
	     (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-q)
	       (print-info :q)
	       (set-source-or-destination-orientation :q))
	     (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-w)
	       (print-info :w)
	       (set-source-or-destination-orientation :w))
	     
	     (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-e)
	       (print-info :e)
	       (set-source-or-destination-orientation :e))
	     (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-r)
	       (print-info :r)
	       (set-source-or-destination-orientation :r))

	     (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-t)
	       (print-info :t)
	       (set-source-or-destination-orientation :t))
	     (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-z)
	       (print-info :y)
	       (set-source-or-destination-orientation :y))
	     (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-y)
	       (print-info :y)
	       (set-source-or-destination-orientation :y))
	     (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-u)
	       (print-info :u)
	       (set-source-or-destination-orientation :u))	     


	     ;; rotate camera horizontally around target
	     (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-j)
	       (decf (glm:vec. *sphere-cam-rel-pos* :x) 1.125))
	     (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-l)
	       (incf (glm:vec. *sphere-cam-rel-pos* :x) 1.125))
	     ;; rotate cam vertically around target
	     (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-i)
	       (decf (glm:vec. *sphere-cam-rel-pos* :y) 1.125))
	     (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-k)
	       (incf (glm:vec. *sphere-cam-rel-pos* :y) 1.125))
	     ;; zoom camera in/out of target
	     ;; (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-u)
	     ;;   (decf (glm:vec. *sphere-cam-rel-pos* :z) 1.5))
	     (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-o)
	       (incf (glm:vec. *sphere-cam-rel-pos* :z) 1.5))


	     (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-space)
	       (if *slerp-toggle*
		   (progn (setf *slerp-toggle* NIL)
			  (print-info))
		   (progn (setf *slerp-toggle* t)
			  (print-info)))
	       
	       )

	     (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
	       (sdl2:push-event :quit)))
	    (:quit () t)
	    (:idle ()
		   ;; MAIN LOOP:

		   ;; preventing special cases (camera transformation):
		   (setf (glm:vec. *sphere-cam-rel-pos* :y)
			 (glm:clamp (glm:vec. *sphere-cam-rel-pos* :y) -78.75 1.0))
		   (setf (glm:vec. *cam-target* :y)
			 (if (> (glm:vec. *cam-target* :y) 0.0)
			     (glm:vec. *cam-target* :y)
			     0.0))
		   ;; can't zoom in "through" the floor with camera
		   (setf (glm:vec. *sphere-cam-rel-pos* :z)
			 (if (> (glm:vec. *sphere-cam-rel-pos* :z) 5.0)
			     (glm:vec. *sphere-cam-rel-pos* :z)
			     5.0))		 

		   ;;rendering code:
		   (display)
		   ;;deviation from arc code for the sake of simple code; still
		   ;; showcasing the concept introduced: quaternion interpolation,
		   ;; linear and spherical
		   (alpha-update)
		   (if *slerp-toggle*
		       (test-slerp)
		       (test-lerp))

		   ;;live editing enabled:
		   (arc::update-swank)
		   
		   (sdl2:gl-swap-window win) ; wow, this can be forgotten easily -.-
		   )))))))



;; TODO: definitely try matrix interpolation just to see what it looks like!
