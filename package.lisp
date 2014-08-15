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
<<<<<<< HEAD
=======
   #:create-program-and-return-it
>>>>>>> first commit
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
<<<<<<< HEAD
=======

(defpackage #:arc-3.1
  (:use :cl)
  (:export #:main))

;; TODO, this is getting ridiculously repetitive
;; the main rendering is what really differs, the solution will be probably
;; to use sdl2kit
(defpackage #:arc-3.2
  (:use :cl)
  (:export #:main))
>>>>>>> first commit
