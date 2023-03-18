(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-csharp-generator")
  (ql:quickload "cl-ppcre")
  (ql:quickload "cl-change-case"))


(in-package :cl-csharp-generator)

(let ((project "LogConf"))
  (defparameter *relative-source-dir* #P"example/10_logconf/source01/LogConf/")
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
			   (:name Microsoft.Extensions.Configuration :version 7.0.0)
			   (:name Microsoft.Extensions.Configuration.CommandLine :version 7.0.0)
			   (:name Microsoft.Extensions.Configuration.Json :version 7.0.0)
			   (:name Microsoft.Extensions.Configuration.Binder :version 7.0.0)
			   (:name Microsoft.Extensions.Logging :version 7.0.0)
			   (:name Microsoft.Extensions.Logging.Console :version 7.0.0)
			   (:name Microsoft.Extensions.DependencyInjection :version 7.0.0)
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

	(progn
	  (out "<ItemGroup>")
	  (out "<None Include=\"appSettings.json\" CopyToOutputDirectory=\"PreserveNewest\" />")
	  (out "</ItemGroup>"))
	(out "</Project>"))))

  (let ((l-conf `((:name executable :short x :type string :default (string "/usr/bin/ls") :required nil)
		  (:name log-file :short f :type string :default (string "") :required nil)
		  (:name debug-level :short l :type string :default (string "") :required nil)
		  ;(:name config-file :short c :type string :default (string "") :required nil :config nil)
		  ;(:name help :short h :type flag :default false :required nil :config nil)
		  )
		))
   (let ((name 'Program))
     (write-source
      (merge-pathnames
       (format nil "~a.cs" name)
       *source-dir*)
     
      `(do0
	(using	Microsoft.Extensions.Configuration
		Microsoft.Extensions.Logging
		Microsoft.Extensions.DependencyInjection)
	(do0
	 (namespace
	  ,(format nil "~a" project)

	  (do0
	   (space-n
	    public interface IConfig
	    (progn
	      ,@(loop for e in l-conf
		      collect
		      (destructuring-bind (&key name short type default required (config t)) e
			(format nil "~a ~a { get; }"
				type
				(cl-change-case:pascal-case (format nil "~a" name)))))))
	   (defclass Config (IConfig)
	     (declare (public))
	     ,@(loop for e in l-conf
		     collect
		     (destructuring-bind (&key name short type default required (config t)) e
		       (format nil "public ~a ~a { get; set; }"
			       type
			       (cl-change-case:pascal-case (format nil "~a" name)))))))
	  
	  (defclass ,name ()

	    

	    (defmethod BuildServiceProvider ()
	      (declare (static)
		       (values IServiceProvider))
	      (let ((collection (new (ServiceCollection))))
		(comments

		 "In this C# project, the `ServiceProvider` is used to facilitate dependency injection. Essentially, services are considered dependencies and can be added to the service collection with different lifetimes. 

There are three types of lifetimes available: `scoped`, `transient`, and `singleton`. 

- `transient` means that a new instance of the service is created every time it is requested from the `ServiceProvider`. 
- `scoped` means that the same instance of the service is returned within a specific scope defined by `using (var scope = serviceProvider.CreateScope()) {...}`, but different instances are returned in other scopes. 
- `singleton` always returns the same instance of the service. It is important to note that any operations performed by the singleton service must be thread-safe."
		 )

		(let ((configuration (new (dot (ConfigurationBuilder)
					      (AddJsonFile (string "appSettings.json")
							   "optional: false")
					      (Build))))))
		(space IConfig (= config null))
		(collection.AddSingleton<IConfig> config)
		(return (collection.BuildServiceProvider))))
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

	       
	       (let ((serviceProvider (BuildServiceProvider))))
	      
	       )))))))))
  
  (sb-ext:run-program "/usr/bin/dotnet"
		      `("format")
		      :directory *source-dir*
		      :wait t)
  
  )
