(in-package #:framework)

;; in c++ "namespace" is very similar to the way packages are used in cl namespace foo {
;; int i = 2 } seems to be equivalent to (in-package :foo) (defparameter i 2). As we
;; access it foo::i in cl we do (foo::i) or if exported (foo:i)

(defun deg-to-rad (ang-degree)
  "Transform degree, expecting as float, into radians"
  (let ((deg-to-rad
	 (/ (* 3.14159 2.0)
	    360.0)))
    (* ang-degree deg-to-rad)))



(defparameter *xml-path*
  (merge-pathnames #p "7-chapter/data/UnitCubeColor.xml"
		   (asdf/system:system-source-directory :arcsynthesis)))


;; cxml-stp test:

(defparameter m-stp (cxml:parse *xml-path* (stp:make-builder)))

(defclass mesh ()
  ((attributes :accessor attrs) ;; usually two attrs objects: positions + colors
   (indices :accessor inds)
   (stp-obj :accessor so)
   (vaos :accessor vaos))) ;; number of indices objects decide number of vbos

(defclass attribute ()
  ((index :accessor index) (type :accessor attr-type) (size :accessor size)
   (data :accessor data)))

(defclass indices ()
  ((command :accessor cmd) (type :accessor indices-type) (data :accessor data)))

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

(defun list-element-nodes (stp-obj)
  (let ((acc (list)))
    (stp:do-recursively (node stp-obj acc)
      (if (typep node 'stp:element)
	  (push node acc)))))

;;;ATTRIBUTES
(defun node->attribute (attr-node)
  (let ((attr (make-instance 'attribute)))
    (stp:with-attributes
	((i "index") (ty "type") (s "size")) attr-node
      (setf (index attr) (read-from-string i))
      ;; TODO: if xml is "ufloat" -falsly- it also returns cl::ufloat!!! ... why?
      (setf (attr-type attr) (arc:string->gl-type ty))
      (setf (size attr) (read-from-string s))
      ;; TODO: brute-force solution shows
      (setf (data attr) (list-from-string (stp:data (stp:first-child attr-node)))))
    attr))

(defun node-attributes-list (element-nodes)
  "Take list of element nodes and return a list of attribute objects from it"
  (let ((stp-attributes
	 (loop for node in element-nodes when
	      (string-equal "attribute" (stp:local-name node))
	    collecting node)))
     	(mapcar #'node->attribute stp-attributes)))

(defun stp-obj->attributes (stp-obj)
  (node-attributes-list (list-element-nodes stp-obj)))

;;;INDICES
(defun node->index (indx-node)
  (let ((indx (make-instance 'indices)))
    (stp:with-attributes
	((c "cmd") (ty "type")) indx-node
      ;;(print *package*) ==> #<PACKAGE "COMMON-LISP-USER"> !
      (setf (cmd indx) (arc:string->gl-type c))
      (setf (indices-type indx) (arc:string->gl-type ty))
      (setf (data indx) (list-from-string (stp:data (stp:first-child indx-node))))
      )
    indx))

(defun node-indices-list (element-nodes)
  "Take list of element nodes and return a list of indices objects from it"
  (let ((stp-indices
	 (loop for node in element-nodes when
	      (string-equal "indices" (stp:local-name node))
	    collecting node)))
    (mapcar #'node->index stp-indices)))

(defun stp-obj->indices (stp-obj)
  (node-indices-list (list-element-nodes stp-obj)))
;;;/INDICES


(defun list-from-string (string)
  (with-input-from-string (str string)
	     (loop for line = (read str nil 'eof)
		until (eq line 'eof)
		 collecting line)))

(defun data-from-element (element)
  (list-from-string
   (stp:data (stp:first-child element))))





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

(defvar *vertex-buffer-object*)

  ;; TODO: read attributes in one vertex-buffer-object,
  ;; format with first attribute using type and number from first attribute,
  ;; if 2nd attribute exists, calculate offset from first
  ;;;
  ;; if one index list, then use it on both attributes. On the first as is,
  ;; on the 2nd by offset.
  ;; If there are two: use first on first attribute, and 2nd on 2nd
  ;;; could be that each index, must belong to separate vbo... hence
  ;; we'd have to create mulitple vbos, and (render mesh) would draw
  ;; each one in order? e.g. UnitCylinder "indices" seem to hold 60 items, just like
  ;; the one attribute provided


;; TODO: fully implement? If yes, move to "auxiliary-functions"
(defun gl-type-byte-size (type)
  (case type
    (:float 4)
    (t (progn (format t "type: ~a unknown! Defaulting to byte-size 4" type)
		 4))))

(defun calc-color-data-offset (position-data-attribute)
  (let* ((pda position-data-attribute) #|->|#  (number-of-vertices (length (data pda)))
	 (type (attr-type pda)) #|->|# (type-size (gl-type-byte-size type)))
    ;; (* #|size-of(float):|# 4 3 *number-of-vertices*)
    (* type-size number-of-vertices)))

(defun indices->index-buffer-objects (indices)
  (print 'hi)
  (loop for inds in indices
     with ibo = (first (gl:gen-buffers 1))
     with index-data = (arc::create-gl-array-of-unsigned-short-from-vector
			(apply #'vector (data inds)))
       ;; NEXT-TODO: why can't call (data inds) inside LOOP, but no problem outside?
     do
       (gl:bind-buffer :element-array-buffer ibo)
       (gl:buffer-data :element-array-buffer :static-draw index-data)
       (gl:bind-buffer :element-array-buffer 0)
     collecting ibo))

(defun build-vaos (mesh vertex-buffer-object)
  ;; TODO: create multiple vaos or call (gl:buffer-data ..) to supply index-data in
  ;; sequence?
  (let ((color-data-offset (calc-color-data-offset (first (attrs mesh))))
	(index-buffer-objects (indices->index-buffer-objects (inds mesh))))
    (loop for ibo in index-buffer-objects
       with vao = (gl:gen-vertex-array) do
	 (gl:bind-vertex-array vao)
	 (gl:bind-buffer :array-buffer vertex-buffer-object)
	 (loop for attribute in (attrs mesh)
	    for data-offset = 0 then color-data-offset
	    do
	      (%gl:enable-vertex-attrib-array (index attribute))
	      (%gl:vertex-attrib-pointer (index attribute)
					 (size attribute)
					 (attr-type attribute) :false 0
					 data-offset))
;       (%gl:bind-buffer :element-array-buffer *index-buffer-object*)
;	 (print ibo)
       ;; (%gl:bind-vertex-array 0)
	 )))

(defun build-vaos-in-mesh (mesh)
  (let* ((vbo (first (gl:gen-buffers 1)))
	 )
    (gl:bind-buffer :array-buffer vbo)
    (gl:buffer-data :array-buffer :static-draw (vertex-data (so mesh)))
    (gl:bind-buffer :array-buffer 0)

    (build-vaos mesh vbo)
    
))


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

;; (defun initialize-vertex-array-objects ()
;;   (setf *vao* (first (gl:gen-vertex-arrays 1)))
;;   (gl:bind-vertex-array *vao*)
;;   (gl:bind-buffer :array-buffer *vertex-buffer-object*)
;;   (%gl:enable-vertex-attrib-array 0)
;;   (%gl:enable-vertex-attrib-array 1)
;;   (%gl:vertex-attrib-pointer 0 3 :float :false 0 0)
;;   (%gl:vertex-attrib-pointer 1 4 :float :false 0 color-data-offset)
;;   (%gl:bind-buffer :element-array-buffer *index-buffer-object*)
;;   (%gl:bind-vertex-array 0))
;;(%gl:draw-elements :triangles (gl::gl-array-size *index-data*)
;;		       :unsigned-short 0)



;; TODO: implement class, with RENDER generic function

(defun make-mesh (stp-obj)
  (let ((mesh (make-instance 'mesh)))
    ;; TODO: ugly ad-hoc solution (n-reverse...) because data in reverse order
    ;; and the vaos building code likes to iterate through it the other way around..
    (setf (attrs mesh) (nreverse (stp-obj->attributes stp-obj)))
    (setf (inds mesh) (stp-obj->indices stp-obj))
    (setf (so mesh) stp-obj)
    (build-vaos-in-mesh mesh)
mesh))

(defun mesh->vao (path-to-xml)
  (let* ((stp-obj  (cxml:parse path-to-xml (stp:make-builder)))
	(mesh (make-mesh stp-obj)))
    mesh))



;; (arc-7:main)

;;render: keywords: cmd
;;(%gl:draw-elements :triangles (gl::gl-array-size *index-data*)
;;		       :unsigned-short 0)
;; :triangle-fan , :triangle-strip

