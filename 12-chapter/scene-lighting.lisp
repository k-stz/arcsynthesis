;;; this tutorial c++ code is spread over several files:
;;; all share these files:
;;; 1. Scene.h and Scene.cpp
;;;    set up the objects in the scene and render them
;;;    contains the surface properties (called _material propeties_)
;;;    they are set in Scene.cpp's GetMaterials() function and include:
;;;    diffuse-color, specular-color (remember metals reflect their own color, but
;;;    most materials diffuse all the colors of the light-source) and
;;;    specular-shininness

;;; 2. Light.h and Light.cpp --


;;; Important c++ note: float padding[3] represent _3_ floats NOT 4 (we don't count
;;; from 0 in this case)!!

;; tell the compiler to not care about speed, use maximum type saftey and
;; give us maximum debug information
;; TODO: sbcl specifics?
(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package #:arc-12)

;; TODO: this might solve the problem:
;; (print (uiop/lisp-build:current-lisp-file-pathname)) ?
(defvar *data-directory*
  (merge-pathnames
   #p "12-chapter/data/" (asdf/system:system-source-directory :arcsynthesis)))

;;todo: fix this output to slime-repl solution
(defvar out *standard-output*)  (defvar dbg *debug-io*) (defvar err *error-output*)


;; so that old code works again:
(defun make-program-data ()
  (make-instance 'program-data))

(defclass unlit-prog-data ()
  ((the-program :accessor the-program)

   (object-color-unif :accessor object-color-unif)
   (model-to-camera-matrix-unif :accessor model-to-camera-matrix-unif)))

(defconstant +material-block-index+ 0)
(defconstant +light-block-index+ 1)
(defconstant +projection-block-index+ 2)

(defun load-unlit-program (str-vertex-shader str-fragment-shader)
  "Create unlit-prog-data object from shader strings."
  (let ((shader-list (list))
	(data (make-instance 'unlit-prog-data))
	(projection-block))
    (push (arc:create-shader
	   :vertex-shader
	   (arc:file-to-string (merge-pathnames str-vertex-shader *data-directory*)))
	  shader-list)
    (push (arc:create-shader
	   :fragment-shader
	   (arc:file-to-string (merge-pathnames str-fragment-shader *data-directory*)))
    	  shader-list)
    ;; settings slots of program-object:
    (setf (the-program data) (arc:create-program shader-list))
    (setf (model-to-camera-matrix-unif data)
	  (gl:get-uniform-location (the-program data) "modelToCameraMatrix"))
    (setf (object-color-unif data)
	  (gl:get-uniform-location (the-program data) "objectColor"))
    
    (setf projection-block
	  ;; TODO: get cl-opengl version containing this version
	  ;;(gl:get-uniform-block-index (the-program data) "Projection")
	  (cffi:with-foreign-string (s "Projection")
	    (%gl:get-uniform-block-index (the-program data) s)))
    (%gl:uniform-block-binding
     (the-program data) projection-block +projection-block-index+)
    data))

(defun load-lit-program (str-vertex-shader str-fragment-shader)
  "Create program-data object from shader strings. Hardcoded uniform reference."
  (let ((shader-list (list))
	(data (make-program-data))
	(material-block)
	(light-block)
	(projection-block))
    (push (arc:create-shader
	   :vertex-shader
	   (arc:file-to-string (merge-pathnames str-vertex-shader *data-directory*)))
	  shader-list)
    (push (arc:create-shader
	   :fragment-shader
	   (arc:file-to-string (merge-pathnames str-fragment-shader *data-directory*)))
    	  shader-list)

    (setf (the-program data) (arc:create-program shader-list))
    (setf (model-to-camera-matrix-unif data)
	  (gl:get-uniform-location (the-program data) "modelToCameraMatrix"))
    (setf (normal-model-to-camera-matrix-unif data)
	  (gl:get-uniform-location (the-program data) "normalModelToCameraMatrix"))
    
    (setf material-block
	  (cffi:with-foreign-string (s "Material")
	    (%gl:get-uniform-block-index (the-program data) s)))    
    (setf light-block
	  (cffi:with-foreign-string (s "Light")
	    (%gl:get-uniform-block-index (the-program data) s)))
    (setf projection-block
	  ;; TODO: get cl-opengl version containing this version
	  ;;(gl:get-uniform-block-index (the-program data) "Projection")
	  (cffi:with-foreign-string (s "Projection")
	    (%gl:get-uniform-block-index (the-program data) s)))

    ;; TODO: remove
    (print (list
	    (the-program data) 
	    (model-to-camera-matrix-unif data)
	    (normal-model-to-camera-matrix-unif data)
	    material-block light-block projection-block))

    ;; TODO: if (materialBlock != GL_INVALID_INDEX) "Can be optomized out."
    (%gl:uniform-block-binding
     (the-program data) material-block +material-block-index+)
    (%gl:uniform-block-binding
     (the-program data) light-block +light-block-index+)
    
    (%gl:uniform-block-binding
     (the-program data) projection-block +projection-block-index+)
    data))


(defvar *shader-files*
  ;; (getf (aref *shader-list* 0) :file-vertex-shader) ==> "PCN.vert"
  #((:file-vertex-shader "PCN.vert" :file-fragement-shader "DiffuseSpecular.frag")
    (:file-vertex-shader "PCN.vert" :file-fragement-shader "DiffuseOnly.frag")
    (:file-vertex-shader "PN.vert" :file-fragement-shader "DiffuseSpecularMtl.frag")  
    (:file-vertex-shader "PN.vert" :file-fragement-shader "DiffuseOnlyMtl.frag")))

(defparameter *programs*
  (make-array (length *shader-files*) :initial-contents
	      (loop for i below (length *shader-files*)
		 collect (make-instance 'program-data))))

(defun get-program (lighting-type-key)
  (let ((index
	 (case lighting-type-key
	   (:lp-vert-color-diffuse-specular 0)
	   (:lp-vert-color-diffuse 1)

	   (:lp-mtl-color-diffuse-specular 2)
	   (:lp-mtl-color-diffuse 3))))
    (aref *programs* index)))

(defvar *unlit*)

(defun initialize-program ()
  (loop for i-prog below (length *programs*) do
       (setf (aref *programs* i-prog)
	     (load-lit-program (getf (aref *shader-files* i-prog) :file-vertex-shader)
			       (getf (aref *shader-files* i-prog) :file-fragement-shader))))

  (setf *unlit* (load-unlit-program "PosTransform.vert" "UniformColor.frag")))


;;TODO: move to "scene.lisp"
(defun scene ()
  (let ((terrain-mesh (framework:xml->mesh-obj (merge-pathnames *data-directory* "Ground.xml")))
	(cube-mesh (framework:xml->mesh-obj (merge-pathnames *data-directory* "UnitCube.xml")))
	(tetra-mesh (framework:xml->mesh-obj (merge-pathnames *data-directory* "UnitTetrahedron.xml")))
	(cyl-mesh (framework:xml->mesh-obj (merge-pathnames *data-directory* "UnitCylinder.xml")))
	(sphere-mesh (framework:xml->mesh-obj (merge-pathnames *data-directory* "UnitSphere.xml"))))

    ;; draw object
    
    terrain-mesh))

(defvar *terrain-mesh* NIL)


(defparameter *projection-uniform-buffer* 0)
(defparameter *light-uniform-buffer* 0)


(defun init ()
  (initialize-program)

  (setf *terrain-mesh* (scene))

  
  (gl:enable :cull-face)
  (%gl:cull-face :back)
  (%gl:front-face :cw) 
  (gl:viewport 0 0 500 500)

  (gl:enable :depth-test)
  (gl:depth-mask :true)
  (%gl:depth-func :lequal)
  (gl:depth-range 0.0 1.0)
  (gl:enable :depth-clamp)

  ;; setup our uniform buffers
  (setf *projection-uniform-buffer* (first (gl:gen-buffers 1)))
  (gl:bind-buffer :uniform-buffer *projection-uniform-buffer*)
  (%gl:buffer-data :uniform-buffer #|sizeof(mat4):|# 64 (cffi:null-pointer) :dynamic-draw)

  (setf *light-uniform-buffer* (first (gl:gen-buffers 1)))
  (gl:bind-buffer :uniform-buffer *light-uniform-buffer*)
  (%gl:buffer-data :uniform-buffer #|sizeof(lightblock):|#
		   160 (cffi:null-pointer) :dynamic-draw)

  ;;"bind the static buffer"
  (%gl:bind-buffer-range :uniform-buffer +projection-block-index+
			 *projection-uniform-buffer* 0 #|sizeof(mat4):|# 64)

  ;; The LightBlock:
  ;; vec4 ambientIntensity  (+ (* 4 4)	 
  ;; float lightAttenuation    4		 
  ;; float padding[3]           (* 3 4)	 ;; just 3 float because we do not count from 0!!
  ;; PerLight lights[4]         (* 4		 
  ;; PerLights: 2xvec4            (* 2 4 4)))
  ;; sizeof(lightblock) ==> 160
  (%gl:bind-buffer-range :uniform-buffer +light-block-index+
  			 *light-uniform-buffer* 0 #|sizeof(lightblock):|# 160)

  
  (gl:bind-buffer :uniform-buffer 0))


(defparameter *view-pole*
  (make-instance 'glutil:view-pole :cam-pos (glm:vec3 0.0 0.8 8.0)
		 ;; calculate trasformation relative to the look-pt
		 ;; for now changes calc-matrix behaviour
		 :trans-mode :camera-relative))


;; initialobjectdata position: 0.0 0.5 0.0
;;                   orientation: (quaternion 1.0 0.0 0.0 0.0)
(defparameter *objt-pole*
  (make-instance 'glutil:object-pole
		 :pos (glm:vec3 0.0 0.5 0.0)
		 :orient (glm:quaternion 1.0 0.0 0.0 0.0)))

(defparameter *draw-colored-cyl* t)
(defparameter *draw-light* nil)


(defparameter *light-height* 1.5)
(defparameter *light-radius* 1.0)

(defparameter *rotate-light-p* t)
(defparameter *world-light-pos-save* (glm:vec4 0.0 0.0 0.0 1.0))


(defun calc-light-position ()
  (let ((curr-time-through-loop
	 (/ (sdl2:get-ticks) 10000.0))
	(ret (glm:vec4 0.0 *light-height* 0.0 1.0)))
    (setf (glm:vec. ret :x) (* (cos (* curr-time-through-loop
				       (* (coerce pi 'single-float) 2.0)))
			       *light-radius*))
    (setf (glm:vec. ret :z) (* (sin (* curr-time-through-loop
				       (* (coerce pi 'single-float) 2.0)))
			       *light-radius*))
    (when *rotate-light-p*
      (setf *world-light-pos-save* ret))
    (if *rotate-light-p* 
	ret
	*world-light-pos-save*)))


(defparameter *light-model* :lm-blinn-specular)

(defparameter *light-attenuation* 1.2)
;; TODO: add jumping between gaussian-specular (0,1] and other
;; lighting models [0,infinity) shininess-factor. solved by
;; arc using the MaterialParams class changing its operator
;; (decides what the instanciated object will evaluate to)
;; based on the *light-model* used.
(defparameter *shininess-factor* 0.5) ;; was 4.0 by default

(defparameter *dark-color* (glm:vec4 0.2 0.2 0.2 1.0))
(defparameter *light-color* (glm:vec4 1.0))
(defparameter *draw-dark-p* NIL)

(defparameter *scale-cyl-p* NIL)


;; TODO: get it to change depending on sdl2:get-ticks
(defparameter *sun-light-direction* (glm:vec3 0.0 -1.0 0.0))
(defun get-light-information (world-to-camera-mat)

  )

(defparameter *test-array* (light-block-test-array))

(defun draw ()
  (let* ((model-matrix (make-instance 'glutil:matrix-stack))
	 (world-to-camera-mat)
	 (light-data ;;for now the initform data shall suffice
	  (make-instance 'light-block)))

    (glutil:set-matrix model-matrix (glutil:calc-matrix *view-pole*))
    
    (setf world-to-camera-mat (glutil:top-ms model-matrix))

    ;; supply data to the Light uniform buffer. Since it is a uniform buffer
    ;; object we're dealing with we don't care what gl:use-program is used!

    (gl:bind-buffer :uniform-buffer *light-uniform-buffer*)
    ;; hm this is bad, AS-GLARR will alloocate a fresh array every time :I
    ;; (gl:buffer-sub-data :uniform-buffer (as-glarr light-data))
    (%gl:buffer-sub-data :uniform-buffer 0 160 *test-array*)

    (gl:bind-buffer :uniform-buffer 0)

    ;; attempt to render ground
    ;; from drawobject:

    ;;(%gl:bind-buffer-range :uniform-buffer +material-block-index+ )


    (glutil:with-transform (model-matrix)
	(let ((norm-matrix (sb-cga:transpose-matrix
			    (sb-cga:inverse-matrix
			     (glutil:top-ms model-matrix))))
	      (program (get-program :lp-vert-color-diffuse)))

	  ;; NEXT-TODO: continue drawobject/draw porting
	  (list norm-matrix)
	  (gl:use-program (the-program program))
	  :rotate 90.0
	  
	  (gl:uniform-matrix (model-to-camera-matrix-unif program) 4
	  		     (vector (glutil:top-ms model-matrix)) NIL)
	  (gl:uniform-matrix (normal-model-to-camera-matrix-unif program) 3
	  		     (vector (glm:mat4->mat3 norm-matrix)) NIL)

	  (framework:render *terrain-mesh*)
	  ))
    

    


    ;; (glutil:with-transform (model-matrix)

    ;; 	;; Render the ground plane
    ;; 	(glutil:with-transform (model-matrix)
    ;; 	    (gl:use-program (the-program p-white-prog))

    ;; 	  (let ((norm-matrix
    ;; 		 (sb-cga:transpose-matrix
    ;; 		  (sb-cga:inverse-matrix
    ;; 		   (glutil:top-ms model-matrix)))))

    ;; 	    (gl:uniform-matrix (model-to-camera-matrix-unif p-white-prog) 4
    ;; 			       (vector (glutil:top-ms model-matrix)) NIL)

    ;; 	    (gl:uniform-matrix (normal-model-to-camera-matrix-unif p-white-prog) 3
    ;; 			       (vector (glm:mat4->mat3 norm-matrix)) NIL)

    ;; 	    (framework:render *plane-mesh*)
    ;; 	    (gl:use-program 0)))

    ;;   ;; Render the Cylinder
    ;;   (glutil:with-transform (model-matrix)
    ;; 	  (glutil:apply-matrix model-matrix (glutil:calc-matrix *objt-pole*))
    ;; 	(when *scale-cyl-p*
    ;; 	  (glutil::scale model-matrix (glm:vec3 1.0 1.0 0.2)))

    ;; 	(let ((norm-matrix
    ;; 	       (sb-cga:transpose-matrix
    ;; 		(sb-cga:inverse-matrix
    ;; 		 (glutil:top-ms model-matrix))))
    ;; 	      (p-prog (if *draw-colored-cyl* p-color-prog p-white-prog)))

    ;; 	  (gl:use-program (the-program p-prog))
    ;; 	  (gl:uniform-matrix (model-to-camera-matrix-unif p-prog) 4
    ;; 			     (vector (glutil:top-ms model-matrix)) NIL)
    ;; 	  (gl:uniform-matrix
    ;; 	   (normal-model-to-camera-matrix-unif p-prog) 3
    ;; 	   (vector (glm:mat4->mat3 norm-matrix)) NIL)

    ;; 	  (if *draw-colored-cyl*
    ;; 	      (framework:render-mode *cylinder-mesh* "lit-color")
    ;; 	      ;;else
    ;; 	      (framework:render-mode *cylinder-mesh* "lit")))
    ;; 	(gl:use-program 0))
    ;;   ;; Render the light
    ;;   (when *draw-light*
    ;; 	(glutil:with-transform (model-matrix)
    ;; 	    ;; TODO: another weakness of WITH-TRANSFORM can't accept
    ;; 	    ;;       objects evaluating to vectors as input
    ;; 	    (glutil::translate model-matrix world-light-pos)
    ;; 	  :scale 0.1 0.1 0.1

    ;; 	  (gl:use-program (the-program *unlit*))
    ;; 	  (gl:uniform-matrix (model-to-camera-matrix-unif *unlit*) 4
    ;; 			     (vector (glutil:top-ms model-matrix)) NIL)
    ;; 	  (gl:uniformfv (object-color-unif *unlit*)
    ;; 			(glm:vec4 0.8078 0.8706 0.9922 1.0))
    ;; 	  (framework:render-mode *cube-mesh* "flat"))))
    ))

(defun display ()
  ;; TODO: bkg to fake changing daytime sky color
  (gl:clear-color 0.0 0.0 0.2 1)
  (gl:clear-depth 1.0)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (draw))

(defparameter *fz-near* 1.0)
(defparameter *fz-far* 1000.0)

(defun reshape (w h)
  (let ((pers-matrix (make-instance 'glutil:matrix-stack)))
    (glutil:perspective pers-matrix 45.0 (/ w h) *fz-near* *fz-far*)

    (gl:bind-buffer :uniform-buffer *projection-uniform-buffer*)
    (gl:buffer-sub-data :uniform-buffer
		      (arc:create-gl-array-from-vector (glutil:top-ms pers-matrix)))
    (gl:bind-buffer :uniform-buffer 0))
  (%gl:viewport 0 0 w h))


(defun mouse-rel-transform (xrel yrel)
  "Allow to look around with the mouse, just like in egoshooters."
  (glutil:rotate-vp :y xrel *view-pole*)
  (glutil:rotate-vp :x yrel *view-pole*))


(defun lmb-pressed-p (state)
  (= state 1))

(defun main ()
  (arc:with-main
    (sdl2:with-init (:everything)
      (progn (setf *standard-output* out) (setf *debug-io* dbg) (setf *error-output* err))
      (sdl2:with-window (win :w 500 :h 500 :flags '(:shown :opengl :resizable))
	(sdl2:with-gl-context (gl-context win)
	  ;; INIT code:
	  (init)
	  (reshape 500.0 500.0)
	  (sdl2:with-event-loop (:method :poll)
	    (:mousewheel
	     (:y y)			; stores the vertical mousewheel motion

	     ;; zoom in/out
	     (when (= y 1)
	       (glutil:move-camera *view-pole*
				   (glutil:pole-direction
				    *view-pole*
				    (glm:vec3 0.0 0.0 -5.0)))
	       (format t "pos:~a~%~%"
		       (glm:round-obj (glutil:cam-pos *view-pole*) 0.001)))
	     (when (= y -1)
	       (glutil:move-camera *view-pole*
				   (glutil:pole-direction
				    *view-pole*
				    (glm:vec3 0.0 0.0 5.0)))
	       (format t "pos:~a~%~%"
		       (glm:round-obj (glutil:cam-pos *view-pole*) 0.001))))
	    
	    (:mousemotion
	     (:xrel xrel :yrel yrel :state state)
	     ;;(:x x :y y :xrel xrel :yrel yrel :state state)
	     ;; and that's all we need to build the arc viewpole: xrel, yrel
	     ;; store the motion relative to the last event!
	     ;; (format t "x:~a y:~a xrel:~a yrel:~a STATE:~a TRANS:~a~%"
	     ;; x y xrel yrel state (glutil:trans-relative-to *view-pole*))
	     (when (lmb-pressed-p state)
	       (mouse-rel-transform xrel yrel)))

	    ;; resizing logic:
	    (:windowevent
	     (:type type :timestamp ts :window-id wi :event ev :padding1 p1
		    :padding2 p2 :padding3 p3 :data1 d1 :data2 d2)
	     ;; according to the output gathered:
	     ;; :event ev either 6 or 5 seems to indicate resizing taking place
	     ;; at which time data1 and data2 hold the new dimension of window
	     ;; while resizing takes place event 6 and 5 both have the same
	     ;; data1 data2 entries
	     ;; (format t "type:~a ts:~a wi:~a ev:~a p1:~a p2:~a p3:~a d1:~a d2:~a~%"
	     ;; 	     type ts wi ev p1 p2 p3 d1 d2)
	     (list type ts wi ev p1 p2 p3 d1 d2)
	     (when (= ev 6) ;; magic number 6 is the signal of the resizing event
	       (reshape (float d1) (float d2))))
	    (:keydown
	     (:keysym keysym)
     	     (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-i)
	       (incf *light-height* 0.2))
	     (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-k)
	       (decf *light-height* 0.2))
	     (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-l)
	       (incf *light-radius* 0.2))
	     (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-j)
	       (decf *light-radius* 0.2))

     	     (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-o)
	       ;; make the surface smoother
	       (incf *shininess-factor* 0.1)
	       (format t "Shiny: ~a~%" *shininess-factor*))
     	     (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-u)
	       ;; make the surface rougher
	       (decf *shininess-factor* 0.1)
	       (format t "Shiny: ~a~%" *shininess-factor*))
	     
	     (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-y)
	       ;; toggle light rendering
	       (setf *draw-light* (not *draw-light*)))
	     (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-z)
	       ;; toggle light rendering
	       (setf *draw-light* (not *draw-light*)))
     	     (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-t)
	       ;; toggle cylinder scaling
	       (setf *scale-cyl-p* (not *scale-cyl-p*)))

	     (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-b)
	       ;; break rotation
	       (setf *rotate-light-p* (not *rotate-light-p*)))

	     (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-g)
	       ;; doggle dark/light plane
	       (setf *draw-dark-p* (not *draw-dark-p*)))
	     
	     (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-h)
	       ;; cycle through lighting-models
	       (case *light-model*
		 (:lm-phong-specular (setf *light-model* :lm-phong-only))
		 (:lm-phong-only (setf *light-model* :lm-blinn-specular))
		 (:lm-blinn-specular (setf *light-model* :lm-blinn-only))
		 (:lm-blinn-only (setf *light-model* :lm-gaussian-specular))
		 (:lm-gaussian-specular (setf *light-model* :lm-gaussian-only))
		 (:lm-gaussian-only (setf *light-model* :lm-phong-specular)))
	       (format t "Lighting Model used: ~a~%" *light-model*))	     

	     (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-space)
	       ;; toggle color on cylinder
	       (setf *draw-colored-cyl* (not *draw-colored-cyl*)))

	     (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
	       (sdl2:push-event :quit)))
	    (:quit () t)
	    (:idle ()
		   ;; MAIN LOOP:
		   ;; clamping: (remove it and witness a nice eye-candy effect)
		   (when (<= *shininess-factor* 0.0)
		     (setf *shininess-factor* 0.0001))

		   ;;rendering code:
		   (display)

		   ;;live editing enabled:
		   (arc:update-swank)
		   (sdl2:gl-swap-window win))))))))
