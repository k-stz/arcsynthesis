(defpackage "TEST"
  (:use :cl))
(in-package #:test)

(defvar *1st*)
(defvar *keystates*)



(defun foo () )

(defun keystate-at (keycode)
  (cffi:mem-ref *keystates* :int keycode))

(defun pushed-p (keycode)
  (/= 0
     (keystate-at keycode)))


;;mouse

(defvar *mousestates*)
(defvar *1st-mouse*)

(defun main ()
  (arc::with-main
    (sdl2:with-init (:everything)
      (sdl2:with-window (win :w 500 :h 500 :flags '(:shown :opengl))
	(sdl2:with-gl-context (gl-context win)
		;;mutliple value, for some reason the 2nd value is the array?
		;;TODO: yeah, time to take a closer look at cffi?

		;;NOTE: cffi:mem-aref access of index in an array
		;;      cffi:mem-ref dereference pointer
		;; SAP= System Area Pointer
		;; this dereferences the pointer: (cffi:mem-ref *keystates* :int )
		
		;; (sdl2-ffi.functions:sdl-get-keyboard-state
		;;  (cffi-sys:null-pointer)))
	  (multiple-value-bind (a arr)
	      (sdl2-ffi.functions:sdl-get-keyboard-state (cffi-sys:null-pointer))
	    (setf *1st* a)
	    (setf *keystates* arr))

	  (multiple-value-bind (a arr)
	      (sdl2-ffi.functions:sdl-get-mouse-state (cffi-sys:null-pointer) (cffi-sys:null-pointer))
	    (setf *1st-mouse* a)
	    (setf *mousestate* arr))
	  
	  (sdl2:with-event-loop (:method :poll)
	    (:keydown
	     (:keysym keysym)
	     (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-e)
	       ;; WORKS: ..but if we test for a key and it is indeed pressed
	       ;;        it returns "1" just like the sdl2 documentation states
	       ;;       we querry something else get a high number instead of 0
	       (print
		(cffi:mem-ref *keystates* :int sdl2-ffi:+sdl-scancode-e+)))

	     (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-space)
	       ;; sdl2-ffi:+sdl-scancode-space+ ==> 44
	       (format t "keysym:~a scancode-val:~a~%" keysym
		       (sdl2:scancode-value keysym))
	       ;; (print 
 	       ;; 	(cffi:mem-ref *keystates* :int sdl2-ffi:+sdl-scancode-space+))
	       )
	     (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
	       (sdl2:push-event :quit)))
	    (:quit () t)
	    (:idle ()

		   ;;; TEST 1
		   ;; (let ((val (loop for i upto 10 when
		   ;; 		   (/= 0 (cffi:mem-ref *keystates* :int i))
		   ;; 		 return i)))
		   ;;   (when val (print val)))

		   ;;; TEST 2
		   ;; WORKS: note how 'f' key can be read via: :int 6 AND 9, could
		   ;; this correspond to keydown/keyup-f??
		   ;; (when (/= 0 (cffi:mem-ref *keystates* :int
		   ;; 			     9))
		   ;;   (print 'reached))
		   ;;

		   ;;; TEST 3
		   ;; This finally seems to prove that 6 and 9 are both testing the
		   ;; same value?
		   ;; (let ((arr-val-1 (keystate-at sdl2-ffi:+sdl-scancode-f+))
		   ;; 	 (arr-val-2 (keystate-at 9)))
		   ;;   (print (list arr-val-1 arr-val-2)))

		   ;;; TEST 4
		   ;; multiple input test,
		   ;; TODO: create simple mapping "lctrl" -> sdl2-ffi:+sdl-scancode-lctrl+
		   (when (and (pushed-p sdl2-ffi:+sdl-scancode-lctrl+)
			      (pushed-p sdl2-ffi:+sdl-scancode-f+))
		     (print 'BOOOM))
		   (arc::update-swank)
		   (sdl2:gl-swap-window win))))))))

;; conslusion: to allow multiple input the :poll event handling is not suitable
;;             next thing to do is to decide if we have to write some convenience
;;             functions on top of sdl2:get-keyboard-state.
;;             Though I don't know yet how to receive the data from :mousemotion
;;             event handler: xrel, yrel.
;;             TODO: test sdl2:sdl-get-mouse-state sdl2:sdl-get-relative-mouse-state

;;             With get-keyboard-state inside :mousemotion event handler should suffice
;;             to implement the arcsynthesis' ViewPole

;; TODO: Now I must be close to figuring out how to listen to :resizeable events!?
