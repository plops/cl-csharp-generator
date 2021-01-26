(declaim (optimize 
	  (safety 3)
	  (speed 0)
	  (debug 3)))

(eval-when (:compile-toplevel :execute :load-toplevel)
     (ql:quickload "cl-csharp-generator")
     (ql:quickload "cl-ppcre"))

(in-package :cl-csharp-generator)


(progn
  (defparameter *source-dir* #P"example/02_helloworld/source/")

  (write-source
   (asdf:system-relative-pathname
    'cl-csharp-generator
    (merge-pathnames
     #P"run_01_helloworld.cs"
     *source-dir*))
   `(do0
     (using System)
     (defclass HelloWorld ()
	    (declare (public))
	    (defmethod Main ()
	      (declare (static)
		       (public))
	      (Console.WriteLine (string "Hello World from Mono!")))))))



;; csc *.cs
;; mono *.exe
