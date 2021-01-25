(declaim (optimize 
	  (safety 3)
	  (speed 0)
	  (debug 3)))

(eval-when (:compile-toplevel :execute :load-toplevel)
     (ql:quickload "cl-csharp-generator")
     (ql:quickload "cl-ppcre"))

(in-package :cl-csharp-generator)


(progn
  (defparameter *source-dir* #P"example/01_first/source/")

  (write-source
   (asdf:system-relative-pathname
    'cl-csharp-generator
    (merge-pathnames
     #P"run_01_winform.cs"
     *source-dir*))
   `(do0
     (using System
	    System.Drawing
	    System.Windows.Forms)
     (class HelloWorld (Form)
	    (declare (public))
	    (defmethod Main ()
	      (declare (static)
		       (public))
	      (Application.Run
	       (new (HelloWorld))))
	    (defmethod HelloWorld ()
	      ;; C++ auto is var in C#
	      (let ((b (new (Button))))
		(setf b.Text (string "Click Me!"))
		(incf b.Click (new (EventHandler ButtonClick)))))
	    (defmethod ButtonClick (sender e)
	      (declare (private)
		       (type object sender)
		       (type EventArgs e))
	      (MessageBox.Show (string "Button Clicked!")))))))



