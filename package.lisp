;;;; package.lisp

(defpackage #:gb-emulator
  (:use #:cl
	#:modest-functional
	#:modest-geometry)
  (:import-from #:modest
		#:r #:g #:b #:a))
