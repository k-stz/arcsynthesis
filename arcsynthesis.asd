;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;TODO: loading this file is causing sbcl to compile functions.. they must be put into the
;;      lisp system (in working memory) i guess. How to disable this by fiat? Well it is
;;      kinda slow.. nah just want to know what's going on in the background there.

;; so you don't have to qualify: (asdf:defsystem ..) just (defsystem ..)
;; this is unnecassary if you load this file using (asdf::load-asd ..)!
;; which is also recommended
(in-package :asdf-user)

(defsystem #:arcsynthesis
  :description "Modern OpenGL example code from the book 'Learning Modern 3D Graphics Programming'
by Jason L. McKesson (website:'www.arcsynthesis.org/gltut')
written in Common Lisp using cl-sdl2 and cl-opengl"
  :version "0.0.1"
  :author "k-stz"
  ;; :licence  TODO
  :depends-on (:cl-opengl :sdl2 :sb-cga :cxml :cxml-stp)
  :serial t		  ; now: :file order in :components order is also dependency order
  :components
  ((:file "package")
   (:file "auxiliary-functions")
   (:file "glm")
   (:file "glutil")
   (:file "framework")
   ;; the following solves the "src in subdirectories" problem nicely!
   (:module "1-chapter/"
   	    :components ((:file "hello-triangle")))
   (:module "2-chapter/"
	    :components ((:file "fragment-color")
			 (:file "vertex-color")))
   (:module "3-chapter/"
   	    :components ((:file "moving-triangle")
   			 (:file "vert-calc-offset")
   			 (:file "vert-position-offset")
			 (:file "frag-change-color")))
   (:module "4-chapter"
	    :components ((:file "ortho-cube")
			 (:file "perspective-projection")
			 (:file "perspective-matrix")
			 (:file "aspect-ratio")))
   (:module "5-chapter"
	    :components ((:file "overlap-no-depth")
			 (:file "base-vertex-with-overlap")
			 (:file "depth-buffering")
			 (:file "vertex-clipping")
			 (:file "depth-clamping")))
   (:module "6-chapter"
	    :components ((:file "translation")
			 (:file "scale")
			 (:file "rotation")
			 (:file "hierarchy")))
   (:module "7-chapter"
	    :components ((:file "world-scene")
			 (:file "world-with-ubo")))
   (:module "8-chapter"
	    :components ((:file "gimbal-lock")
			 (:file "quaternion-YPR")
			 (:file "camera-relative")
			 (:file "interpolation")))
   ))


;; (:file "1-chapter/auxiliary-functions")
   ;; (:file "1-chapter/hello-triangle")))
  
  ;; :components ((:file "package")
  ;; 	       (:file "1-chapter/auxiliary-functions"
  ;; 		      :depends-on ("package"))
  ;; 	       (:file "1-chapter/hello-triangle"
  ;; 		      :depends-on ("1-chapter/auxiliary-functions")

;;TODO: bad style?  
(asdf:load-system :arcsynthesis)
