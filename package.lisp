;; pcl advise ; obsolete once ASDF is used?
;; avoids risk of interning symbols in some other package, from whom this file is loaded
;; or compiled
(in-package "COMMON-LISP-USER")

(defpackage #:arcsynthesis
  (:use :cl)
  (:nicknames :arc)
  (:export ;; helper functions from auxiliary-functions.lisp:
   #:fill-gl-array
   #:create-gl-array-from-vector
   #:create-shader
   #:create-program
   #:create-program-and-return-it
   #:file-to-string
   #:gl-array-content))

(defpackage #:arc-1
  (:documentation "1. tutorial")
  (:use #:cl #:arcsynthesis)
  (:export #:main))
  

(defpackage #:arc-2
  (:use :cl)
  (:export #:main))

(defpackage #:arc-2.1
  (:use :cl)
  (:export #:main))

(defpackage #:arc-3
  (:use :cl)
  (:export #:main))

(defpackage #:arc-3.1
  (:use :cl)
  (:export #:main))

;; TODO, this is getting ridiculously repetitive
;; the main rendering is what really differs, the solution will be probably
;; to use sdl2kit
(defpackage #:arc-3.2
  (:use :cl)
  (:export #:main))


(defpackage #:arc-3.3
  (:documentation "fragChangeColor.cpp") ;;TODO: good organizational idea?
  (:use :cl)
  (:export #:main))

(defpackage #:arc-4
  (:documentation "OrthoCube.cpp")
  (:use :cl)
  (:export #:main))

(defpackage #:arc-4.1
  (:documentation "First Perspective Projection")
  (:use :cl)
  (:export #:main))

(defpackage #:arc-4.2
  (:documentation "Perspective projection using projection Matrix")
  (:use :cl)
  (:export #:main))

(defpackage #:arc-4.3
  (:documentation "Aspect of the World")
  (:use :cl)
  (:export #:main))

;; chapter 5

(defpackage #:arc-5
  (:documentation "Objects in Depth")
  (:use :cl)
  (:export #:main))
 
(defpackage #:arc-5.1
  (:documentation "Optimization: Base Vertex")
  (:use :cl)
  (:export #:main))

(defpackage #:arc-5.2
  (:documentation "Overlap and Depth Buffering")
  (:use :cl)
  (:export #:main))

(defpackage #:arc-5.3
  (:documentation "Boundaries and Clipping")
  (:use :cl)
  (:export #:main))
