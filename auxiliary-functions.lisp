(in-package #:arc)

(defmacro continuable (&body body)
  "Helper macro that we can use to allow us to continue from an
  error. Remember to hit C in slime or pick the restart so errors don't kill the
  app."
  `(restart-case
       (progn ,@body) (continue () :report "Continue")))

(defun update-swank ()
  "Called from within the main loop, this keep the lisp repl
working while cepl runs"
  (continuable
   (let ((connection
	  (or swank::*emacs-connection*
	      (swank::default-connection))))
     (when connection
       (swank::handle-requests connection t)))))

(defun fill-gl-array (gl-array data-array)
  "Fills gl-array <gl-array> with <data-array> of type cl:array, <data-array>'s contents
will be COERCEd to SINGLE-FLOAT"
  (if (and (typep gl-array 'gl:gl-array)
	   (arrayp data-array)
           (= (gl::gl-array-size gl-array) ;wow, this important function's an internal?
	      (length data-array)))
      (dotimes (i (length data-array))
	(setf
	 (gl:glaref gl-array i)
	 (coerce (aref data-array i) 'single-float)))
      (progn
	(print "couldn't fill gl-array, either size of gl-array and data-array don't")
      (print "match or they aren't of proper type"))))

(defun create-gl-array-from-vector (vector-of-floats)
  (let* ((array-length (length vector-of-floats))
	 (gl-array (gl:alloc-gl-array :float array-length)))
    (fill-gl-array gl-array vector-of-floats)
    gl-array))

;;not fully implemented
(defun create-gl-array-of-type-from-vector (v type)
  (flet ((fill-gl-array ()
           ;; TODO: how to fill it with arbitrary type?
	   )))
  (let* ((array-length (length v))
	 (gl-array (gl:alloc-gl-array type array-length)))
    (fill-gl-array gl-array v)
    gl-array))

;;TODO: replace this function by a general purpose one:
(defun create-gl-array-of-short-from-vector (vector-of-shorts)
  (flet ((fill-gl-array-of-short (gl-array)
	   (dotimes (i (length vector-of-shorts))
	     (setf
	      (gl:glaref gl-array i)
	      (coerce (aref vector-of-shorts i) '(signed-byte 16))))))
    (let* ((array-length (length vector-of-shorts))
	   (gl-array (gl:alloc-gl-array :short array-length)))
      (fill-gl-array-of-short gl-array)
      gl-array)))

(defun create-gl-array-of-unsigned-short-from-vector (vector-of-unsigned-shorts)
  (flet ((fill-gl-array-of-short (gl-array)
	   (dotimes (i (length vector-of-unsigned-shorts))
	     (setf
	      (gl:glaref gl-array i) (aref vector-of-unsigned-shorts i)))))
    (let* ((array-length (length vector-of-unsigned-shorts))
	   (gl-array (gl:alloc-gl-array :unsigned-short array-length)))
      (fill-gl-array-of-short gl-array)
      gl-array)))


;;TODO: combine many shader objects just by repeatedly calling this?
;;      why (list const-string-shader-file), is it for program of multiple string or
;;      for multiple one string programs?
;;; ATTENTION: can only be used when OpenGL context exists "somewhere" lol
(defun create-shader (e-shader-type const-string-shader-file)
  "Create and RETURN shader"
  (let ((shader (gl:create-shader e-shader-type)))
    (format t "shader:~a~%" shader) ;why is it always 0
    ;; feed const-string-shader-file into shader object (as in OpenGL object)
    (gl:shader-source shader const-string-shader-file) ;it expects a string-LIST
    (%gl:compile-shader shader)
    ;; was the compilation successful?
    (if (gl:get-shader shader :compile-status)
      (format t "~a compilation successful~%" e-shader-type)
      (format t "Compilation failure in ~a:~% ~a~%"
	      e-shader-type
	      (gl:get-shader-info-log shader)))
    shader))

(defun create-program (shader-list)
   ;;TODO: is this bad style to gl:use it as well?
  "Create program and gl:use-program it."
  (let ((program (%gl:create-program)))
    ;; "attach" all of the created shader objects to the program object
    (loop for shader-object in shader-list
       do (%gl:attach-shader program shader-object))
    ;; I guess links all the attached shaders to actually form a program object
    (%gl:link-program program)
    (if (gl:get-program program :link-status)
	(print "Linking successful! (program object)")
	(print (gl:get-program-info-log program)))
    ;; remove shader objects from program:
    (loop for shader-object in shader-list
       do (%gl:detach-shader program shader-object))
    (print "executing glUseProgram!")
    ;; finally we need to tell OpenGL that rendering commands should use
    ;; our program object instead of its default rendering state:
    ;; arcsyntheses: "it, glUseProgram, is later called with 0 to indicate that no
    ;;                program will be used for rendering
    (%gl:use-program program)
    ))

;; stolen from cbaggers
;;; yeaaaah, no. It is functional style to "return-it" always, so that is a misnomer!
(defun create-program-and-return-it (shader-list)
   ;;TODO: is this bad style to gl:use it as well?
  "Create program and RETURN it"
  (let ((program (%gl:create-program)))
    ;; "attach" all of the created shader objects to the program object
    (loop for shader-object in shader-list
       do (%gl:attach-shader program shader-object))
    ;; I guess links all the attached shaders to actually form a program object
    (%gl:link-program program)
    (if (gl:get-program program :link-status)
	(print "Linking successful! (program object)")
	(print (gl:get-program-info-log program)))
    ;; remove shader objects from program:
    (loop for shader-object in shader-list
       do (%gl:detach-shader program shader-object))
    ;; finally we need to tell OpenGL that rendering commands should use
    ;; our program object instead of its default rendering state:
    ;; arcsyntheses: "it, glUseProgram, is later called with 0 to indicate that no
    ;;                program will be used for rendering
    program
    ))

;; stolen from cbaggers ;;TODO: aha, give an &optional asdf/system:system-source-directory !!
;; and let it merge it with <path> !!
(defun file-to-string (path)
  "Sucks up an entire file from PATH into a freshly-allocated
string, returning two values: the string and the number of
bytes read."
  (with-open-file (s path)
    (let* ((len (file-length s))
	   (data (make-string len)))
      (values data (read-sequence data s)))))

(defun gl-array-content (gl-array)
  "Returns list of gl-array's content."
  (loop for i from 0 below (gl::gl-array-size gl-array) collecting
       (gl:glaref gl-array i)))
