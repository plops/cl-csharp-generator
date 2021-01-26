(declaim (optimize 
	  (safety 3)
	  (speed 0)
	  (debug 3)))

(eval-when (:compile-toplevel :execute :load-toplevel)
     (ql:quickload "cl-csharp-generator")
     (ql:quickload "cl-ppcre"))

(in-package :cl-csharp-generator)

;; https://www.mono-project.com/docs/getting-started/mono-basics/
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


(write-source
   (asdf:system-relative-pathname
    'cl-csharp-generator
    (merge-pathnames
     #P"run_02_https_get.cs"
     *source-dir*))
   `(do0
     (using System
	    System.Net)
     (defclass HttpsTest ()
	    (declare (public))
	    (defmethod Main ()
	      (declare (static)
		       (public))
	      (let ((c (new (System.Net.WebClient))))
		(Console.WriteLine
		 (c.DownloadString (string "https://www.nuget.org"))))))))


(write-source
   (asdf:system-relative-pathname
    'cl-csharp-generator
    (merge-pathnames
     #P"run_03_winforms.cs"
     *source-dir*))
   `(do0
     (using System
	    System.Windows.Forms)
     (defclass WinFormsTest (Form)
	    (declare (public))
	    (defmethod Main ()
	      (declare (static)
		       (public))
	      (Application.Run (new (WinFormsTest))))
	    (defmethod WinFormsTest ()
	      (declare (values :constructor))
	      (setf Text (string "helloworld"))))))

(write-source
   (asdf:system-relative-pathname
    'cl-csharp-generator
    (merge-pathnames
     #P"run_04_gtksharp.cs"
     *source-dir*))
   `(do0
     (using Gtk
	    System
	    )
     (defclass GtkTest ()
	    (declare (public))
	    (defmethod Main ()
	      (declare (static)
		       (public))
	      (Application.Init)
	      (let ((win (new (Window (string "hello mono gtksharp")))))
		(win.Show)
		(Application.Run))))))
;; mcs run_04_gtksharp.cs -pkg:gtk-sharp-2.0


;; csc *.cs
;; mono *.exe
