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
  (defparameter *source-dir* #P"example/03_unity/source/")

  (write-source
   ;; class name has to be same as filename
   "/home/martin/New Unity Project/Assets/Scripts/Player.cs"
   #+nil (asdf:system-relative-pathname
    'cl-csharp-generator
    (merge-pathnames
     #P"01_Player.cs"
     *source-dir*))
   `(do0
     ;; https://youtu.be/pwZpJzpE2lQ?t=2124
     (using System.Collections
	    System.Collections.Generic
	    UnityEngine
	    )
     (defclass Player (UnityEngine.MonoBehaviour)
	    
       (defmethod Start ()
	 )
       (defmethod Update ()
	 (when (Input.GetKeyDown KeyCode.Space)
	   (Debug.Log (string "space pressed."))
	   ))))))
