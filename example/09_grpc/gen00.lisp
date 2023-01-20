(eval-when (:compile-toplevel :execute :load-toplevel)
     (ql:quickload "cl-csharp-generator"))

(in-package :cl-csharp-generator)

;; mkdir source; cd source
;; dotnet --version => 6.0.405
;; dotnet new console --framework net6.0
;; dotnet add package Grpc.AspNetCore

(progn
  (defparameter *source-dir* #P"example/09_grpc/source/")
  (ensure-directories-exist (asdf:system-relative-pathname
			     'cl-csharp-generator
			     *source-dir*))
  (let ((project "grpcExample"))
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

		    (defmethod Configure (app env)
		      (declare (type IApplicationBuilder app)
			       (type IWebHostEnvironment env)
			       (public))
		      (if (env.IsDevelopment)
			  (app.UseDeveloperExceptionPage)
			  )
		      (app.UseRouting)
		      (app.UseEndpoints (lambda (endpoints)
					  (dot endpoints
					       (MapGrpcService<MyServiceImpl>)
					       (EnableGrpcWeb)))))
		    
		      )


		  (defclass MyServiceImpl (MyService.MyService.MyServiceBase)
		    (declare (public))
		    (defmethod SayHello (request context)
		      (declare (public) (override)
			       (values Task<HelloReply>)
			       (type HelloRequest request)
			       (type ServerCallContext context))
		      (return (Task.FromResult
			       (new HelloReply (progn
						 (space-n (string$ "Hello {request.Name}")))))))))))))



