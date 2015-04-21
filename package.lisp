;; pcl advise ; obsolete once ASDF is used?
;; avoids risk of interning symbols in some other package, from whom this file is loaded
;; or compiled
(in-package "COMMON-LISP-USER")

(defpackage #:arcsynthesis
  (:use :cl)
  (:nicknames :arc)
  (:export ;; helper functions from auxiliary-functions.lisp:
   #:continuable
   #:update-swank
   #:with-main
   #:fill-gl-array
   #:create-gl-array-from-vector
   #:create-shader
   #:create-program
   #:create-program-and-return-it
   #:file-to-string
   #:gl-array-content
   #:string->gl-type))

(defpackage #:glm
  (:use :cl)
  (:export
   #:make-mat4
   #:+identity-mat4+
   #:mat4-place
   #:set-mat4
   #:set-mat4-row
   #:set-mat4-col
   #:mat*vec
   #:vec-
   #:vec.
   #:vec3
   #:vec4
   #:round-obj
   #:vec4*
   #:vec4+
   #:normalize
   #:vec4-from-vec3
   #:vec4->vec3
   #:vec3->vec4
   ;;quaternions:
   #:quat
   #:vectorize   
   #:quaternion
   #:make-quat
   #:quat*
   #:quat-cast
   #:mat4-cast
   #:quat-normalize
   #:conjugate-quat
   ;;
   #:set-mat4-diagonal
   #:make-mat3
   #:set-mat3
   #:mat4-from-mat3
   #:mat4->mat3
   #:clamp
   #:mix
   #:slerp
   #:dot4-product
   ;;transformations:
   #:rotate-x
   #:rotate-y
   #:rotate-z
   #:rotate-axis))

(defpackage #:glutil
  (:use :cl)
  ;; TODO export
  (:export
   #:matrix-stack
   #:with-transform
   #:perspective
   #:top-ms
   #:set-matrix
   #:apply-matrix
   #:view-pole
   #:cam-pos
   #:trans-relative-to
   #:rotate-vp
   #:move-camera
   #:pole-direction
   #:object-pole
   #:calc-matrix))

(defpackage #:framework
  (:use :cl)
  ;; TODO export
  (:export
   #:deg-to-rad
   #:render
   #:render-mode
   #:xml->mesh-obj
   #:render-ship
   #:ship-xml->vao))


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

(defpackage #:arc-5.4
  (:documentation "Depth Clamping")
  (:use :cl)
  (:export #:main))

(defpackage #:arc-6
  (:documentation "Translation")
  (:use :cl)
  (:export #:main))

(defpackage #:arc-6.1
  (:documentation "Scale")
  (:use :cl)
  (:export #:main))

(defpackage #:arc-6.2
  (:documentation "Rotation")
  (:use :cl)
  (:export #:main))

(defpackage #:arc-6.3
  (:documentation "Hierarchy model")
  (:use :cl)
  (:export #:main))

(defpackage #:arc-7
  (:documentation "World in Motion")
  (:use :cl)
  (:export #:main))

(defpackage #:arc-7.1
  (:documentation "Shared Uniforms")
  (:use :cl)
  (:export #:main))

(defpackage #:arc-8
  (:documentation "Getting Oriented - Gimbal Lock")
  (:use :cl)
  (:export #:main))

(defpackage #:arc-8.1
  (:documentation "Getting Oriented - Quaternion")
  (:use :cl)
  (:export #:main))

(defpackage #:arc-8.2
  (:documentation "Getting Oriented - Camera Relative Orientation")
  (:use :cl)
  (:export #:main))

(defpackage #:arc-8.3
  (:documentation "Getting Oriented - Interpolation")
  (:use :cl)
  (:export #:main))

;; Part 3 - Illumination
(defpackage #:arc-9
  (:documentation "Lights On - Modelling Lights")
  (:use :cl)
  (:export #:main))

(defpackage #:arc-9.1
  (:documentation "Lights On - Normal Transformation")
  (:use :cl)
  (:export #:main))

(defpackage #:arc-9.2
  (:documentation "Lights On - Global Illumination")
  (:use :cl)
  (:export #:main))

(defpackage #:arc-10
  (:documentation "Lights On - Plane Lights")
  (:use :cl)
  (:export #:main))

(defpackage #:arc-10.1
  (:documentation "Lights On - Fragment Lighting")
  (:use :cl)
  (:export #:main))
