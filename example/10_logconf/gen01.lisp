(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-csharp-generator")
  (ql:quickload "cl-ppcre")
  (ql:quickload "cl-change-case"))


(in-package :cl-csharp-generator)

(let ((project "LogConf"))
  (defparameter *relative-source-dir* #P"example/10_logconf/source01/")
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
	  (loop for e in `(  ;(:name Google.Protobuf :version 3.22.00)
					;(:name Grpc.Net.Client :version 2.51.0)
					;(:name CommandLineParser :version 2.9.1)
			   (:name Microsoft.Extensions :version 7.0.0)
			   (:name Microsoft.Extensions.CommandLine :version 7.0.0)
			   (:name Microsoft.Extensions.Json :version 7.0.0)
			   (:name Microsoft.Extensions.Binder :version 7.0.0)
			   (:name Microsoft.Extensions.Logging :version 7.0.0)
			   (:name Microsoft.Extensions.Logging.Console :version 7.0.0)
			   )
		do
		   (destructuring-bind (&key name version) e
		     (format s "<PackageReference Include=\"~a\" Version=\"~a\" />~%" name version)))
	  #+nil (let ((e `(:name Grpc.Tools :version 2.51.0)))
		  (destructuring-bind (&key name version) e
		    (format s "<PackageReference Include=\"~a\" Version=\"~a\">~%" name version)
		    (out "<IncludeAssets>runtime; build; native; contentfiles; analyzers; buildtransitive</IncludeAssets>")
		    (out "<PrivateAssets>all</PrivateAssets>")
		    (out "</PackageReference>")))
	  (out "</ItemGroup>"))
	(out "</Project>"))))

  (let ((name 'Program))
    (write-source
     (merge-pathnames
      (format nil "~a.cs" name)
      *source-dir*)
     
     `(do0
       (using	Microsoft.Extensions.Configuration
		Microsoft.Extensions.Logging)
       (do0
	(namespace
	 ,(format nil "~a" project)
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
	      
	      ))))))))
  
  (sb-ext:run-program "/usr/bin/dotnet"
		      `("format")
		      :directory *source-dir*
		      :wait t)
  
  )
