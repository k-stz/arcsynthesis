(in-package #:glutil)

(defclass matrix-stack ()
  ((m-curr-mat :initform (glm:make-mat4 1.0)
	       :accessor m-curr-mat)
   ;; is supposed to be a simple stack, so it is just a list here
   (m-matrices :initform (list)
	       :accessor m-matrices)))

;;TODO: with-matrix-stack that deletes object at the end? From what I read,
;; garbage collection facilitation is bad style since it is there to alleviate
;; the programmer of this burden. How to even force object deletion? Probably
;; inefficient to force destruction of object or..??
;;(defun matrix-stack (&optional))

(defgeneric set-matrix (matrix-stack mat4))
(defmethod set-matrix ((ms matrix-stack) (mat4 simple-array))
  "Set the current (top) matrix of the matrix-stack to the given mat4"
  (setf (m-curr-mat ms) mat4))

(defgeneric top-ms (matrix-stack))
(defmethod top-ms ((ms matrix-stack))
  "Returns the current-matrix"
  (m-curr-mat ms))

;; TODO: lock on "push" even though it is cl:push is a function not a generic method
(defgeneric push-ms (matrix-stack))
(defmethod push-ms ((ms matrix-stack))
  "PUSH the current-matrix on the internal stack"
  (push (m-curr-mat ms) (m-matrices ms)))

(defgeneric pop-ms (matrix-stack))
(defmethod pop-ms ((ms matrix-stack))
  "POP last PUSHed matrix and set it to the current-matrix"
  ;; (first (m-matrices ms)) = m-matrices.top() note: m-matrices is supposed to be a
  ;; simple stack!
  (setf (m-curr-mat ms) (first (m-matrices ms)))
  (pop (m-matrices ms)))

;; TODO: defmethod lambda-list displayed with class-name instead of "offset-vec3"
;; it's "simple-array"
(defgeneric translate (matrix-stack simple-array))
(defmethod translate ((ms matrix-stack) (offset-vec3 simple-array))
  "Translate transform the current-matrix by given vec3"
  (let ((translate-mat4 (glm:make-mat4 1.0))
	(vec4 (glm:vec4-from-vec3 offset-vec3)))
    (glm:set-mat4-col translate-mat4 3 vec4)
    (setf (m-curr-mat ms) (sb-cga:matrix* translate-mat4
					  (m-curr-mat ms)))))


(defgeneric rotate-x (matrix-stack float))
(defmethod rotate-x ((ms matrix-stack) (ang-deg float))
  (let ((translate-mat4 (glm:rotate-x ang-deg)))
    ;; NEXT-TODO !!!!!!!!!!                 vvvvvv WRONG ORDER!?!?!?!??!?!?!
    (setf (m-curr-mat ms) (sb-cga:matrix* translate-mat4
					  (m-curr-mat ms)))))

(defgeneric rotate-y (matrix-stack float))
(defmethod rotate-y ((ms matrix-stack) (ang-deg float))
  (let ((translate-mat4 (glm:rotate-y ang-deg)))
    (setf (m-curr-mat ms) (sb-cga:matrix* translate-mat4
					  (m-curr-mat ms)))))

(defgeneric rotate-z (matrix-stack float))
(defmethod rotate-z ((ms matrix-stack) (ang-deg float))
  (let ((translate-mat4 (glm:rotate-z ang-deg)))
    (setf (m-curr-mat ms) (sb-cga:matrix* translate-mat4
					  (m-curr-mat ms)))))


(defgeneric scale (matrix-stack simple-array))
(defmethod scale ((ms matrix-stack) (scale-vec simple-array))
  (let ((scale-mat4 (glm:make-mat4 1.0)))
    (glm:set-mat4-diagonal scale-mat4
  			   (glm:vec4-from-vec3 scale-vec))
    (setf (m-curr-mat ms) (sb-cga:matrix* scale-mat4
					  (m-curr-mat ms)))))

(defun matrix-stack-top-to-shader-and-draw (matrix-stack mat-unif index-data)
      (gl:uniform-matrix mat-unif 4
			 (vector (top-ms matrix-stack)))
      (%gl:draw-elements :triangles (gl::gl-array-size index-data)
			 :unsigned-short 0))

;; (defmacro with-transform ((&key (drawp t)) matrix-stack &body body)
;;   "Creates PUSH-MS POP-MS wrapper around its input, so many with-transform can
;; be nested to facilitate the hierarchical model."
;;   `(progn
;;      (push-ms ,matrix-stack)
;;      ,@body ;; put another with-transform here
;;      ,(when drawp
;; 	    `(matrix-stack-top-to-shader-and-draw ,matrix-stack))
;;      (pop-ms ,matrix-stack)))


;; this is all garbage and doesn't work yet.. maybe need to read on
;; ;;this problem before trying an implementation?
;; (defmacro with-translate2 ((&key (drawp t)) matrix-stack &body body)
;;   `(progn
;;        (push-ms ,matrix-stack)
;;        ,@body
;;        ,(when drawp
;; 	      `(matrix-stack-top-to-shader-and-draw ,matrix-stack))
;;        (pop-ms ,matrix-stack)))


;; (&key (drop t)) removed as having to provide args (matrix-stack-top-to-shader ..) is too
;; specific to abstract away in the macro
(defmacro with-transform ((matrix-stack) &body body)
    "Creates PUSH-MS POP-MS wrapper around its input, so many with-transform can
be nested to facilitate the hierarchical model."
  (labels ((try-key (car l exp)
	     (cond ((null l)
		    (nreverse exp))
		   ;; this ensures that the body may contain arbitrary functions,
		   ;; and only free keywords get special treatment
		   ((listp car) (progn (push car exp)
				       (try-key (cadr l) (cdr l) exp)))
		   ;; keyword special treatment
		   (t (case car
			(:translate (make-trans 'translate (cdr l) exp))
			(:scale (make-trans 'scale (cdr l) exp))
			(:rotate-x (make-rot 'rotate-x (cdr l) exp))
			(:rotate-y (make-rot 'rotate-y (cdr l) exp))
			(:rotate-z (make-rot 'rotate-z (cdr l) exp))
			(t (format *error-output* "~a is not a known transform" car))
			))))
	   (make-trans (key l exp)
	     (push
	      `(,key ,matrix-stack (glm:vec3 ,(first l) ,(second l) ,(third l)))
	      exp)
	     (try-key (cadddr l) (cdddr l) exp))
	   (make-rot (key l exp)
	     (push
	      `(,key ,matrix-stack ,(first l))
	      exp)
	     (try-key (cadr l) (cdr l) exp)))
    ;; start labels body:
    `(progn
       (push-ms ,matrix-stack)
       ,@(try-key (car body) body '())
       (pop-ms ,matrix-stack))
    ))
