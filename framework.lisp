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
  ((command :accessor cmd) (type :accessor indices-type) (data :accessor data)
   (array-size :accessor array-size)))


(defgeneric render (mesh))
(defmethod render ((mesh mesh))
  ;; exploits the fact that the order of indices-obj and vao-obj match with what
  ;; was used to create the vao-obj. Hence related data such as CMD and TYPE can be
  ;; used. For now only CMD is really variable for each vao
  (loop for vao-obj in (vaos mesh)
     for index-obj in (inds mesh) do
       (%gl:bind-vertex-array vao-obj)
     ;; TODO: ugh, again with this long function, create data needed upstream (make-mesh)
       (%gl:draw-elements (cmd index-obj)
			  (array-size index-obj)
			  ;; this was causing heavy memory allocation
			  ;; without being released, even after the demo was closed!
			  ;; solution: store array-size in index-obj upon creation!
			  ;; (gl::gl-array-size
			  ;;  (arc::create-gl-array-of-unsigned-short-from-vector
			  ;;   (apply #'vector
			  ;; 	   (data index-obj))))
			  (indices-type index-obj) 0)
       (%gl:bind-vertex-array 0)
       ))

;;; about cxml-stp:
;; everything is a node, every node has PARENTS
;; (stp:parent m-stp) , some CAN have children:

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
      (setf (data indx) (list-from-string (stp:data (stp:first-child indx-node)))))
    (setf (array-size indx) (index->array-size indx))
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

(defun index->array-size (index)
  (gl::gl-array-size
   (arc::create-gl-array-of-unsigned-short-from-vector
    (apply #'vector (data index)))))

(defun list-from-string (string)
  (with-input-from-string (str string)
	     (loop for line = (read str nil 'eof)
		until (eq line 'eof)
		 collecting line)))

(defun data-from-element (element)
  (list-from-string
   (stp:data (stp:first-child element))))




(defun vertex-data (mesh)
  "Takes all the attributes of a mesh, regardless of multiple occurences of different attribute tags in an xml,
and puts them in a single gl-array in order of appearance of attributes"
  (arc:create-gl-array-from-vector
   (apply #'vector
	  (apply #'append (mapcar #'data (attrs mesh))))) )

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
  (let* ((pda position-data-attribute) #|->|#  (number-of-elements (length (data pda)))
	 (type (attr-type pda)) #|->|# (type-size (gl-type-byte-size type)))
    ;; (* #|size-of(float):|# 4 3 *number-of-vertices*)
    (* type-size number-of-elements)))

(defun indices->index-buffer-objects (indices)
  (let* ((data-list (mapcar #'data indices))
	 (gl-index-data-list ;; TODO: yeah, maybe rewrite this?
	  (mapcar #'arc::create-gl-array-of-unsigned-short-from-vector
		  (mapcar #'(lambda (x) (apply #'vector x)) data-list))))
     
    (loop for ibo in (gl:gen-buffers (length indices)) and
	  index-data in gl-index-data-list
	 ;; TODO: did it work??
       do
       	 (gl:bind-buffer :element-array-buffer ibo)
       	 (gl:buffer-data :element-array-buffer :static-draw index-data)
       	 (gl:bind-buffer :element-array-buffer 0)
       collecting ibo))
  )

(defun build-vaos (mesh vertex-buffer-object)
  ;; TODO: create multiple vaos or call (gl:buffer-data ..) to supply index-data in
  ;; sequence?
  (let ((color-data-offset (calc-color-data-offset (first (attrs mesh))))
	(index-buffer-objects (indices->index-buffer-objects (inds mesh))))
    (loop for ibo in index-buffer-objects
	 ;; attention 'for <var> = <sexp>' will evaluate <sexp> on every iteration!
	 ;; And that's what we need here: new vertex-array-object for each run through
       for vao = (gl:gen-vertex-array) do
	 (gl:bind-vertex-array vao)
	 (gl:bind-buffer :array-buffer vertex-buffer-object)
       ;; this is opengl state setting code
	 (loop for attribute in (attrs mesh)
	    for data-offset = 0 then color-data-offset
	    do ;; (format t "~%~a ~a ~a ~a"  (index attribute) (size attribute)
	       ;; 	       (attr-type attribute) data-offset)
	      (%gl:enable-vertex-attrib-array (index attribute))
	      (%gl:vertex-attrib-pointer (index attribute)
					 (size attribute)
					 (attr-type attribute) :false 0
					 data-offset))
       ;; associate current vao with index-data in index-buffer-object: ibo
	 (%gl:bind-buffer :element-array-buffer ibo)
	 (%gl:bind-vertex-array 0)
	 ;; finally collecting the vao we carefully pieced together :)
       collecting VAO)))

(defun build-vaos-in-mesh (mesh)
  (let* ((vbo (first (gl:gen-buffers 1)))
	 )
    ;; build vbo first
    (gl:bind-buffer :array-buffer vbo)
    (gl:buffer-data :array-buffer :static-draw (vertex-data mesh))
    (gl:bind-buffer :array-buffer 0)
    
    (setf (vaos mesh) (build-vaos mesh vbo))))


;; (defun initialize-vertex-buffer (stp-obj)
;;   (setf *vertex-buffer-object* (first (gl:gen-buffers 1)))

;;   (gl:bind-buffer :array-buffer *vertex-buffer-object*)
;;   (gl:buffer-data :array-buffer :static-draw (vertex-data stp-obj))
;;   (gl:bind-buffer :array-buffer 0)

;;   ;; index-array time:
;;   (setf *index-buffer-object* (first (gl:gen-buffers 1)))

;;   (gl:bind-buffer :element-array-buffer *index-buffer-object*)
;;   (gl:buffer-data :element-array-buffer :static-draw (index-data stp-obj))
;;   (gl:bind-buffer :element-array-buffer  0))

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

(defun xml->mesh-obj (path-to-xml)
  (let* ((stp-obj  (cxml:parse path-to-xml (stp:make-builder)))
	(mesh (make-mesh stp-obj)))
    mesh))


(defvar foo)
;; brute-force, non reusable solution
(defun ship-xml->vao (path-to-ship-xml)
  (let ((mesh (xml->mesh-obj path-to-ship-xml)))
    (setf foo mesh)

    ;;building vbo of first attribute data for test:
    (let ((vbo (first (gl:gen-buffers 1)))
	  (vao (first (gl:gen-vertex-arrays 1))))

      ;; vbo setup
      (gl:bind-buffer :array-buffer vbo)
      (gl:buffer-data :array-buffer :static-draw
		      (arc::create-gl-array-from-vector
		       (apply #'vector (data (first (attrs mesh))))))
      (gl:bind-buffer :array-buffer 0)

      ;; vao setup
      (gl:bind-vertex-array vao)
      (gl:bind-buffer :array-buffer vbo)
      (%gl:enable-vertex-attrib-array (index (first (attrs mesh))))
      (%gl:vertex-attrib-pointer (index (first (attrs mesh)))
				(size (first (attrs mesh)))
				(attr-type (first (attrs mesh))) :false 0
				0)
      ;;unbind vao, that we pieced together
      (gl:bind-vertex-array 0)
      vao) ;; just return the vao
    ))

(defun render-ship (ship-vao)
  (%gl:bind-vertex-array ship-vao)
  (%gl:draw-arrays :triangles 0 336)
  )
  ;; (attrs mesh) and (so mesh) exist, attrs returns list of attributes containing:
  ;; (index ..) => 0, 1 or 3    (type ..) => all three float   (size ..)
  ;;
  ;; build-vaos-in-mesh
  ;; 1. create vbo, fill with attributes of all tags in one buffer
  ;; now call BUILD-VAOS called with mesh and the vbo


;; ship.xml 3 attributes with 336 points
;;          3 index (0,1,2)


;; building two vao from it: 
;;   1."flat" using attrib = 0
;;   2."tint" using attrib = 0 and 1
;;; current assumption:
;;  index 0 attribute = triangle data
;;  index 2 attribute = triangle data
;;  index 1 attribute = color data
;;;; also index 0 = positions , index 1 = colors using a pos-color-local-transformation.vert


;; (arc-7:main)

;;render: keywords: cmd
;;(%gl:draw-elements :triangles (gl::gl-array-size *index-data*)
;;		       :unsigned-short 0)
;; :triangle-fan , :triangle-strip

