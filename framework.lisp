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
