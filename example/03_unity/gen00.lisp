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
       "bool jumpKeyWasPressed;"
       "float horizontalInput;"
       "Rigidbody body;"
       "bool isGrounded;"
					;"public Transform groundCheckTransform;"
       "[SerializeField] private Transform groundCheckTransform = null;"
       
       (defmethod Start ()
	 (setf body (GetComponent<Rigidbody>)))
       (defmethod Update ()
	 (when (Input.GetKeyDown KeyCode.Space)
	   (setf jumpKeyWasPressed true)
	   )
	 (setf horizontalInput (Input.GetAxis (string "Horizontal"))))
       (defmethod FixedUpdate ()
	 ;; once every physics update (100Hz)
	 #+nil (unless isGrounded
		 (return))
	 (when (== 1 (dot (Physics.OverlapSphere groundCheckTransform.position
					    0.1s0)
		     Length))
	   (return))
	 (when jumpKeyWasPressed
	   (setf jumpKeyWasPressed false)
	   (body.AddForce (* 5 Vector3.up)
			  ForceMode.VelocityChange)
	   )
	 (setf body.velocity
	       (new (Vector3 (* 4 horizontalInput) body.velocity.y 0)))
	 )
       (defmethod OnCollisionEnter (c)
	 (declare (type Collision c))
	 (setf isGrounded true)
	 )
       (defmethod OnCollisionExit (c)
	 (declare (type Collision c))
	 (setf isGrounded false)
	 )))))
