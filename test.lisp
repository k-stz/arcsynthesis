(defpackage #:test (:use :cl) (:export :main))
(in-package #:test)

(defun main ()
  (sdl2:with-init (:everything)
	  (progn    (print (bt:current-thread))
	     (print sb-thread:*current-thread*))
	  (print 'coookiiiies)
	  (print 'coooookies)
    (sdl2:with-window (win :flags '(:shown :opengl))
      (sdl2:with-gl-context (gl-context win)
	(sdl2:with-event-loop (:method :poll)
	  ;; press ESC to close window
	  (:keyup
	   (:keysym keysym)
	   (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-e)
              #|experimental code here|#)
	   (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
	     (sdl2:push-event :quit)))
	  ;;problem code:
	  (:idle ()
		 ;; what to put here, so that REPL input works, and we can call
		 ;; OpenGL functions e.g. for testing: (print (gl:gl-version)) to
		 ;; make interactive changes at program runtime?

		 )
	  (:quit () t))))))

;;cbaggers solution that doesn't work, I think because (sdl2:in-main-thread ..) in above
;;macroexpansions? 
(defun update-swank ()
  "Called from within the main loop, this keep the lisp repl
working while cepl runs"
   (let ((connection
	  (or swank::*emacs-connection*
	      (swank::default-connection))))
     (when connection
       (swank::handle-requests connection t))))

(defmacro restartably (&body body)
  `(restart-case
       (progn ,@body)
     (continue () :report "Continue" )))
