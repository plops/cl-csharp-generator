(eval-when (:compile-toplevel :execute :load-toplevel)
     (ql:quickload "cl-csharp-generator"))

(in-package :cl-csharp-generator)

;; mkdir source; cd source
;; dotnet --version => 6.0.405
;; dotnet new console --framework net6.0
;; dotnet add package ProtoBuf

(progn
  (defparameter *source-dir* #P"example/06_tcp_proto/source/")
  (ensure-directories-exist (asdf:system-relative-pathname
			     'cl-csharp-generator
			     *source-dir*))
  (let ((project "TCPProtoBufExample"))
    (write-source
     (asdf:system-relative-pathname
      'cl-csharp-generator
      (merge-pathnames
       (format nil "~a.cs" project)
       *source-dir*))
     `(do0
       (using System
	      System.Threading.Tasks
	      Microsoft.AspNetCore.Builder
	      Microsoft.AspNetCore.Hosting
	      Microsoft.AspNetCore.Http
	      Microsoft.Extensions.DependencyInjection
	      Microsoft.Extensions.Hosting
	      Grpc.Core
	      Grpc.AspNetCore
	      MyService)
       (namespace Server
		  (defclass Program ()
		    (defmethod Main (args)
		      (declare (type "string[]" args)
			       (static))
		      (dot (CreateHostBuilder args)
			   (Build)
			   (Run)))
		    (defmethod CreateHostBuilder (args)
		      (declare (type "string[]" args)
			       (values IHostBuilder)
			       (public)
			       (static))
		      (let ((hostBuilder (Host.CreateDefaultBuilder args)))
			(hostBuilder.ConfigureWebHostDefaults
			 (lambda (webBuilder)
			   (webBuilder.UseStartup<Startup>)))
			(return hostBuilder))

		      
		      ))
		  (defclass Startup ()
		    (declare (public))

		    (defmethod ConfigureServices (services)
		      (declare (type IServiceCollection services)
			       (public))
		      (services.AddGrpc))
		      ))))))



