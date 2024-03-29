(eval-when (:compile-toplevel :execute :load-toplevel)
     (ql:quickload "cl-csharp-generator"))

(in-package :cl-csharp-generator)

;; cd source
;; dotnet --version => 6.0.405
;; dotnet new console --framework net6.0
;; dotnet add package Avalonia

;; dotnet new --install Avalonia.Templates
;; dotnet new avalonia.app -o MyApp
;; cd MyApp
;; dotnet run

;; dotnet tool install -g NuGet.CommandLine
;; nuget install Avalonia -ProjectPath source.csproj

(progn
  (defparameter *source-dir* #P"example/04_avalonia/source/")
  (ensure-directories-exist (asdf:system-relative-pathname
			     'cl-csharp-generator
			     *source-dir*))
  (let ((project "MainWindow"))
    (write-source
     (asdf:system-relative-pathname
      'cl-csharp-generator
      (merge-pathnames
       (format nil "~a.cs" project)
       *source-dir*))
     `(do0
       (using Avalonia
	      Avalonia.Controls
	      Avalonia.Markup.Xaml)
       (namespace MyApp			;,(format nil "~a" project)
		  (defclass MainWindow (Window)
		    (declare
		     (public)
		     )
		    (defmethod MainWindow ()
		      (declare 
		       (public)
		       (values :constructor))
		      (InitializeComponent)
					;(this.AttachDevTools)
		      )
	 
		    (defmethod InitializeComponent ()
		      (declare (private))
		      (AvaloniaXamlLoader.Load this)))
		  (defclass Program ()
		    (defmethod Main (args)
		      (declare (type "string[]" args)
			       (static))
		      (dot AppBuilder
			   (Configure<App>)
			   (UsePlatformDetect)
			   (Start<MainWindow>)))))))))



