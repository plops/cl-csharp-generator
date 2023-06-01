(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-csharp-generator")
  (ql:quickload "cl-ppcre")
  (ql:quickload "cl-change-case"))


(in-package :cl-csharp-generator)

(let ((project "SfmlExample"))
  (defparameter *relative-source-dir* (format nil "example/12_sfml/source01/~a/" project))
  (defparameter *source-dir* (asdf:system-relative-pathname
			     'cl-csharp-generator
			     *relative-source-dir*))
  (ensure-directories-exist *source-dir*)
  (defparameter *day-names*
    '("Monday" "Tuesday" "Wednesday"
      "Thursday" "Friday" "Saturday"
      "Sunday"))
  (defun lprint (&key (msg "") vars)
      `(Console.WriteLine
	(string$ ,(format nil "~a~{ ~a~}"
			  msg
			  (loop for e in vars
				collect
				(format nil "~a='{~a}'"
					(emit-cs :code e)
					(emit-cs :code e)))))))
  (let ((fn (merge-pathnames
	     (format nil  "~a.csproj" project)
	     *source-dir*)))
    (with-open-file (s fn
		       :direction :output
		       :if-exists :supersede)
      (format t "~a~%" fn)
      (flet ((out (cmd)
	       (format s "~a~%" cmd)))
	(out "<Project Sdk=\"Microsoft.NET.Sdk\">")
	(out "<PropertyGroup>")
	(loop for (e f) in `((OutputType Exe)
			     (TargetFramework net6.0)
			     (ImplicitUsings enable)
			     (Nullable enable)
					;(IncludeAllContentForSelfExtract true)
					;(TrimMode link)
					;(PublishTrimmed true)
					;(PublishSingleFile true)
					;(InvariantGlobalization true)
					;(IlcOptimizationPreference Size)
					;(IlcFoldIdenticalMethodBodies true)
					;(IlcDisableReflection true)
					;(IlcGenerateStackTraceData false)
			     )
	      do
		 (format s "<~a>~a</~a>~%" e f e))
	(out "</PropertyGroup>")
       
	#+nil
	(progn
	  (out "<ItemGroup>")
	  (loop for e in `(DataProtocol)
		do
		   (format s "<Protobuf Include=\"..\\..\\proto\\~a.proto\" GrpcServices=\"Client\" />~%" e))
	  (loop for e in `(ExternalDLL)
		do
		   (format s "<Reference Include=\"~a\"><HintPath>artifacts\\~a.dll</HintPath></Reference>~%" e e))
	  (loop for e in `("OtherLibrary\\OtherLibrary.csproj")
		do
		   (format s "<ProjectReference Include=\"~a\" />~%" e))
	 
	  (out "</ItemGroup>"))

	(progn
	  (out "<ItemGroup>")
	  (loop for e in `( 			   
			   (:name SFML.Net :version 2.5.0)
			   )
		do
		   (destructuring-bind (&key name version) e
		     (format s "<PackageReference Include=\"~a\" Version=\"~a\" />~%" name version)))
	  (loop for e in `(;(:name Grpc.Tools :version 2.51.0)
			    (:name StyleCop.Analyzers :version 1.1.118))
		 do
		    (destructuring-bind (&key name version) e
		      (format s "<PackageReference Include=\"~a\" Version=\"~a\">~%" name version)
		      (out "<IncludeAssets>runtime; build; native; contentfiles; analyzers; buildtransitive</IncludeAssets>")
		      (out "<PrivateAssets>all</PrivateAssets>")
		      (out "</PackageReference>")))
	  (out "</ItemGroup>"))

	
	(out "</Project>"))))1

  (let ((l-conf `((:name executable :short x :type string? :default (string "/usr/bin/ls") :required nil)
		  (:name log-file :short f :type string? ; :default (string "")
		   :required nil)
		  (:name debug-level :short l :type string? ;:default (string "")
		   :required nil
			 )
		  (:name config-file :short c :type string? :default (string "appSettings.json") :required nil :config nil)
		  (:name host-name :short h :type string? ;:default (string "")
		   :required nil :config nil)
		  (:name port :short p :type int? :required nil :config nil)
		  ;(:name help :short h :type bool :default false :required nil :config nil)
		  )
		))
   (let ((name 'Program))
     (write-source
      (merge-pathnames
       (format nil "~a.cs" name)
       *source-dir*)
     
      `(do0
	(comments 
		  "<copyright file=\"Program.cs\" company=\"Plops\">"
		  "Copyleft (c) 2023 Plops. This file is MIT Licensed."
		  "</copyright>"
		  )
	
	(do0
	 (namespace
	  ,(format nil "~a" project)

  	  (using				; System
	 SFML.Graphics
	 SFML.System
	 SFML.Window)
	  ""
	  (defclass ,name ()
	    
	    (defmethod Main (args)
	      (declare (type "string[]" args)
		       (static))
	      (do0
	       ,(lprint :msg (multiple-value-bind
				   (second minute hour date month year day-of-week dst-p tz)
				 (get-decoded-time)
			       (declare (ignorable dst-p))
			       (format nil "code generation on: ~2,'0d:~2,'0d:~2,'0d of ~a, ~d-~2,'0d-~2,'0d (GMT~@d)"
				       hour
				       minute
				       second
				       (nth day-of-week *day-names*)
				       year
				       month
				       date
				       (- tz))))

	       
	       
	       (let ((window (new (RenderWindow (new (VideoMode 800 600))
						(string "SFML.Net Window"))))
		     (circle (new (CircleShape 50f))))
		 (setf circle.FillColor Color.Red
		       
		       circle.Position (new (Vector2f 50f 50f)))
		 (let ((clock (new (Clock)))
		       (baseRadius 50f)
		       (pulseSpeed (cast float (/ (* 2 Math.PI )
						  1.3))))

		     (while window.IsOpen
			    (window.DispatchEvents)
			    (window.Clear)
			    (let ((time (clock.ElapsedTime.AsSeconds)))
			     (setf circle.Radius
				   (* baseRadius
				      (/ (+ 1 (cast float
						  (Math.Sin (* pulseSpeed
							       time))))
					 2))))
			    (window.Draw circle)
			    (window.Display))))
	      
	       )))))))))
  
  #+nil (sb-ext:run-program "/usr/bin/dotnet"
		      `("format")
		      :directory *source-dir*
		      :wait t)
  
  )
