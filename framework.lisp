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
   (vao-tags :accessor vao-tags)
   (vaos :accessor vaos))) ;; number of indices objects decide number of vbos

(defclass vao-tag ()
  ((mode-name :accessor mode-name)
   (source-attributes :accessor source-attributes)))

;;TODO: when to use set (set-pprint-dispatch.. ) ? DEFMETHOD less efficient?
;;PRINT-OBJECT: is for print representation of objects, while SET-PPRINT-DISPATCH
;;is for pretty-printing, note how set-pprint-dispatch format is ignored once
;; the *pretty-print* variable is set to NIL!
(defmethod print-object ((vt vao-tag) stream)
  ;; TODO: there is some macro code that allows to print-unreadably, also it may
  ;; be that user defined PRINT-OBJECT must follow some strict rules that are
  ;; covered by said macro (automatically puts #<...> around objects
  (format stream "#<VAO-TAG:[mode:~a]>" (mode-name vt)))

(defclass attribute ()
  ((index :accessor index) (type :accessor attr-type) (size :accessor size)
   (data :accessor data)))

(defmethod print-object ((ab attribute) stream)
  (format stream "#<ATTR:[index=~a]>" (index ab)))

(defclass indices ()
  ((command :accessor cmd) (type :accessor indices-type) (data :accessor data)
   (array-size :accessor array-size)))


(defgeneric render (mesh))
(defmethod render ((mesh mesh))
  ;; exploits the fact that the order of indices-obj and vao-obj match with what
  ;; was used to create the vao-obj. Hence related data such as CMD and TYPE can be
  ;; used. For now only CMD is really variable for each vao
  (loop for vao-obj in (getf (vaos mesh) 'default)
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
       (%gl:bind-vertex-array 0)))

(defgeneric render-mode (mesh mode))
(defmethod render-mode ((mesh mesh) mode)
  (loop for vao-obj in (getf (vaos mesh) (intern (string-upcase mode)))
     for index-obj in (inds mesh) do
       (%gl:bind-vertex-array vao-obj)
       (%gl:draw-elements (cmd index-obj)
			  (array-size index-obj)
			  (indices-type index-obj) 0)
       (%gl:bind-vertex-array 0)))


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
      ;; TODO: if a variable "s" is defined this macro expansion fails!
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
  (let ((attr-list
	 (node-attributes-list (list-element-nodes stp-obj))))
    ;; mmh look at this juicy code! solves the problem of
    ;; having index gaps or being out of order, with this
    ;; we can provide the data to a vbo and calculate offset
    ;; and have them be ordered by INDEX in the .xml attributes
    ;; beware, SORT is destructive
    (sort attr-list (lambda (x y) (< (index x) (index y))))))


;;VAO-TAGS

(defun node->vao-tag (vao-tag-node)
  (flet ((node-source-list (vao-tag-node)
	   (let* ((src-nodes (list-element-nodes vao-tag-node))
		  (source-list
		   (loop for node in src-nodes when
		      ;; theye 
			(string-equal "source" (stp:local-name node))
		      collecting node)))
	     ;; not sure if order matters or not, so we reverse to get the textuall
	     ;; order of the .xml representation
	     (nreverse source-list))))
    (let ((src-list (node-source-list vao-tag-node))
	  (vto (make-instance 'vao-tag)))
      (stp:with-attributes ((name "name")) vao-tag-node
	(setf (mode-name vto) name))
      (setf (source-attributes vto) 
	    (loop for node in src-list collecting
		 (stp:with-attributes ((attrib "attrib")) node
		   (handler-case (parse-integer attrib)
		     (parse-error ()
		       (format *error-output* 
			       "~a is not a string representing an integer
provided *.xml file's vao tags may not contain integer attrib tags" attrib))))))
      vto)))


(defun node-vao-tag-list (element-nodes)
  "Take list of element nodes and return a list of vao-tag objects from it"
  (let ((stp-vao-tags
	 (loop for node in element-nodes when
	      (string-equal "vao" (stp:local-name node))
	    collecting node)))
     	(mapcar #'node->vao-tag stp-vao-tags)))



(defun stp-obj->vao-tags (stp-obj)
  (node-vao-tag-list (list-element-nodes stp-obj)))

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
  "Takes all the attributes of a mesh and puts them in a single gl-array, the attributes
are sorted in order of their index"
  (arc:create-gl-array-from-vector
   (apply #'vector
	  (apply #'append (mapcar #'data (attrs mesh))))))


(defvar *index-buffer-object*)
(defvar *index-data*)
(defun index-data (stp-obj)
  (arc::create-gl-array-of-unsigned-short-from-vector
   (apply #'vector
	  (data-from-element (get-element "indices" stp-obj)))))


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
       do
       	 (gl:bind-buffer :element-array-buffer ibo)
       	 (gl:buffer-data :element-array-buffer :static-draw index-data)
       	 (gl:bind-buffer :element-array-buffer 0)
       collecting ibo)))


(defun build-vaos (mesh vbo)
  (let ((vaos (list 'default (build-vao-of-attr-indx mesh (mapcar #'index (attrs mesh)) vbo))))
    (when (vao-tags mesh)
      (loop for vt in (vao-tags mesh) 
	 do (progn
	      (push (build-vao-of-attr-indx mesh (source-attributes vt) vbo)
		    vaos)
	      ;; somewhat awkward solution: "color" -> "COLOR" -> symbol: COLOR
	      (push (intern (string-upcase (mode-name vt))) vaos))))
    vaos))

(defun build-vao-of-attr-indx (mesh attr-indices vbo)
  (let ((offsets-hash (attr-offsets-hash mesh))
	(index-buffer-objects (indices->index-buffer-objects (inds mesh))))

    (loop for ibo in index-buffer-objects
       ;; attention 'for <var> = <sexp>' will evaluate <sexp> on every iteration!
       ;; And that's what we need here: new vertex-array-object for each run through
       for vao = (gl:gen-vertex-array) do
	 (gl:bind-vertex-array vao)
    	 (gl:bind-buffer :array-buffer vbo)
       ;; this is opengl state setting code
	 (loop for attr-index in attr-indices
	    for attribute = (find-if #'(lambda (x) (= (index x) attr-index)) (attrs mesh))
	    for data-offset = (gethash (index attribute) offsets-hash)
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

(defun attr-offsets-hash (mesh)
  "Return a hash-table containing the vbo memory offset for
each attribute index data, using the index as key for retrival."
  (let ((attr-byte-size-list
	 (loop for attr in (attrs mesh)
	    :for number-of-elements = (length (data attr))
	    :for type = (attr-type attr)
	    :for type-size = (gl-type-byte-size type)
	    :collecting (* type-size number-of-elements)))
	(offsets-list))
    (setf offsets-list
	  ;; (loop for offset-size in attr-byte-size-list
	  ;;    ;; sum the values values iterated over so far
	  ;;    ;; into the variable "acc", just what we want here
	  ;;    :sum offset-size into acc
	  ;;    :collecting acc)
	  (loop for i in attr-byte-size-list
	     with result = (list 0)
	     sum i into acc
	     do (push acc result)
	     finally (return (nreverse (rest result)))))
    ;; finally to access the values in a meaningful way, we'll make them
    ;; accessible via the attributes INDEX, as key in the hashtable
    ;; TODO: penalties of using hash-tables?
    (let ((hash (make-hash-table)))
      (loop for attr in (attrs mesh)
	 :for offset in offsets-list
	 :do
	   (setf (gethash (index attr) hash) offset))
      hash)))
  

(defun build-vaos-in-mesh (mesh)
  (let* ((vbo (first (gl:gen-buffers 1))))
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


;;1. vbo = gen-buffer, bind-buffer, buffer-data = (all attrs data), unbind
;;2. access hetegenous vbo data calculating *-offsets; build index-buffer-objects

(defun make-vbo-accessor (mesh)
  (let ((vbo (first (gl:gen-buffers 1))))
    (gl:bind-buffer :array-buffer vbo)
    (gl:buffer-data :array-buffer :static-draw (vertex-data mesh))
    (gl:bind-buffer :array-buffer 0)))

(defun make-mesh (stp-obj)
  (let ((mesh (make-instance 'mesh)))
    ;; TODO: ugly ad-hoc solution (n-reverse...) because data in reverse order
    ;; and the vaos building code likes to iterate through it the other way around..
    (setf (attrs mesh) (stp-obj->attributes stp-obj))
    (setf (inds mesh) (stp-obj->indices stp-obj))
    (setf (vao-tags mesh) (stp-obj->vao-tags stp-obj))
    (setf (so mesh) stp-obj)

    (build-vaos-in-mesh mesh)
    mesh))

(defun xml->mesh-obj (path-to-xml)
  (let* ((stp-obj  (cxml:parse path-to-xml (stp:make-builder)))
	(mesh (make-mesh stp-obj)))
    mesh))



;; brute-force, non-reusable solution
(defun ship-xml->vao (path-to-ship-xml)
  (let ((mesh (xml->mesh-obj path-to-ship-xml)))

    ;;building vbo of first attribute data for test:
    (let ((vbo (first (gl:gen-buffers 1)))
	  (vao (first (gl:gen-vertex-arrays 1))))

      ;; vbo setup
      (gl:bind-buffer :array-buffer vbo)
      (gl:buffer-data :array-buffer :static-draw
		      ;; (arc::create-gl-array-from-vector
		      ;;  (apply #'vector (data (first (attrs mesh)))))
		      (vertex-data mesh))

      (gl:bind-buffer :array-buffer 0)

      ;; vao setup
      (gl:bind-vertex-array vao)
      (gl:bind-buffer :array-buffer vbo)
      ;; position data
      (%gl:enable-vertex-attrib-array (index (first (attrs mesh))))
      (%gl:vertex-attrib-pointer (index (first (attrs mesh)))
				(size (first (attrs mesh)))
				(attr-type (first (attrs mesh))) :false 0
				0)
      ;; color data
      (%gl:enable-vertex-attrib-array (index (third (attrs mesh))))
      (%gl:vertex-attrib-pointer (index (third (attrs mesh)))
				 (size (third (attrs mesh)))
				 (attr-type (third (attrs mesh))) :false 0
				 ;; number of vertices (336) x float-size (4) x points per
				 ;; vertex (3) = 4032; and we want to read the 3rd
				 ;; attribute block, which all are of equal size, (note
				 ;; that the vbo holds all the attribute data of the
				 ;; Ship.xml in the sequence in which it was provided) so
				 ;; we multiply by two, skipping the first two:
				 ;; 4032x2=8064
				 8064
				 ;; try lower values for funky colors, maybe more suitable
				 ;; as they make for a more distinguishable object. Actual
				 ;; vbo data is on the range: [0,12096] note: 4032 will be
				 ;; read to draw the ship (gl:draw-arrays :triangles 0
				 ;; 336), so the highest value to take, so we don't read
				 ;; default black colors, is in fact 8064 which is also
				 ;; where the supposedly for use color data is
				 )
      ;;the VAO is now pieced in the opengl context vao-working-bench (binding via
      ;;gl:bind-vertex-array), and can be now unbound
      (gl:bind-vertex-array 0)
      vao) ;; just return the vao
    ))


;; UPDATE: the <vao > tags implicate which attributes to use (by index)! 
(defun render-ship (ship-vao)
  (%gl:bind-vertex-array ship-vao)
  (%gl:draw-arrays :triangles 0 336)
  (%gl:bind-vertex-array 0))
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

;;------------------------------------------------------------------------------
;; New lisp-data geometry files approach:

;;; POSTPONED: working on xml solution integrating the <vao> tag
;; (defvar *lst* 0)

;; (defun lisp-data->vao (path-to-lisp-data)
  
;;   (let ((the-end (gensym "-nil"))
;; 	(lst-data))
;;     (setf lst-data
;; 	  (with-open-file (str path-to-lisp-data)
;; 	    (loop for sexp = (read str nil the-end)
;; 	       with lst = ()
;; 	       if (not (eql the-end sexp))
;; 	       :do (push sexp lst)
;; 	       else
;; 	       :return (nreverse lst))))
;;     (setf *lst* lst-data)))

;; (arc-9:main)

;; VAO organization:
