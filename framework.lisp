(in-package #:framework)

;; in c++ "namespace" is very similar to the way packages are used in cl namespace foo { int i = 2 }
;; seems to be equivalent to (in-package :foo) (defparameter i 2). As we access it foo::i in cl we do
;; (foo::i) or if exported (foo:i)

(defun deg-to-rad (ang-degree)
  "Transform degree, expecting as float, into radians"
  (let ((deg-to-rad
	 (/ (* 3.14159 2.0)
	    360.0)))
    (* ang-degree deg-to-rad)))


;; (defclass mesh ()
;;   ((vbo :accessor vbo :initform (first (gl:gen-buffers 1)))
;;    (buffer-data :accessor buffer-data)))


;; (defgeneric bind-buffer (mesh gl-array))

;; CXML test, (cxml is licensed under lisp-lgpl)

(defparameter *xml-path*
  (merge-pathnames #p "7-chapter/data/UnitPlane.xml"
		   (asdf/system:system-source-directory :arcsynthesis)))


(defparameter *list-from-xml* (cxml:parse-file *xml-path* (cxml-xmls:make-xmls-builder)))

(defparameter dom (cxml:parse-file *xml-path* (cxml-dom:make-dom-builder)))
(defparameter ddom (dom:document-element dom))


;; cxml-stp test:

(defparameter m-stp (cxml:parse *xml-path* (stp:make-builder)))

;; everything is a node, every node has PARENTS
;; (stp:parent m-stp) , some CAN have children:

;; TODO: rewrite; this is trial-and-error code
;; to avoid procrastination a "brute-force" solution was implemented

;; access attributes with: (stp:attribute-value <element> "attribute-name")
(defun get-element (element-string stp-obj)
  (stp:find-recursively element-string stp-obj
			:key (lambda (node)
			       (if (typep node 'cxml-stp:element)
				   (cxml-stp:local-name node)))
			:test 'string-equal))

(defun list-from-string (string)
  (with-input-from-string (str string)
	     (loop for line = (read str nil 'eof)
		until (eq line 'eof)
		 collecting line)))

(defun data-from-element (element)
  (list-from-string
   (stp:data (stp:first-child element))))



(defvar *vertex-buffer-object*)
(defun vertex-data (stp-obj)
  ;; check out this APPLY use!! Could solve all problems of the sort
  ;; (append (list 1) (list 2)) `(1 ,@b 2) ??
  (arc:create-gl-array-from-vector
   (apply #'vector
	  (data-from-element (get-element "attribute" stp-obj)))))

(defvar *index-buffer-object*)
(defvar *index-data*)
(defun index-data (stp-obj)
  (arc::create-gl-array-of-unsigned-short-from-vector
   (apply #'vector
	  (data-from-element (get-element "indices" stp-obj)))))

(defun initialize-vertex-buffer (stp-obj)
  (setf *vertex-buffer-object* (first (gl:gen-buffers 1)))

  (gl:bind-buffer :array-buffer *vertex-buffer-object*)
  (gl:buffer-data :array-buffer :static-draw (vertex-data stp-obj))
  (gl:bind-buffer :array-buffer 0)

  ;; index-array time:
  (setf *index-buffer-object* (first (gl:gen-buffers 1)))

  (gl:bind-buffer :element-array-buffer *index-buffer-object*)
  (gl:buffer-data :element-array-buffer :static-draw (index-data stp-obj))
  (gl:bind-buffer :element-array-buffer  0))


;; (defvar *vao*)
;; (defun initialize-vertex-array-objects ()
;;   (setf *vao* (first (gl:gen-vertex-arrays 1)))
;;   (gl:bind-vertex-array *vao*)

;;   (let ((color-data-offset (* #|size-of(float):|# 4 3 *number-of-vertices*)))
;;     (gl:bind-buffer :array-buffer *vertex-buffer-object*)
;;     (%gl:enable-vertex-attrib-array 0)
;;     (%gl:enable-vertex-attrib-array 1)
;;     (%gl:vertex-attrib-pointer 0 3 :float :false 0 0)
;;     (%gl:vertex-attrib-pointer 1 4 :float :false 0 color-data-offset)
;;     (%gl:bind-buffer :element-array-buffer *index-buffer-object*)

;;     (%gl:bind-vertex-array 0)
;;     ;; unbind element-array-buffer? since it already, received data, and
;;     ;; the *vao* implicit setting is done?
;; ))

;; TODO: implement class, with RENDER generic function
(defun make-mesh (path-to-xml)
  (let ((stp-obj  (cxml:parse path-to-xml (stp:make-builder))))
    (initialize-vertex-buffer stp-obj)
    ))
