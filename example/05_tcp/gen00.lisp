(eval-when (:compile-toplevel :execute :load-toplevel)
     (ql:quickload "cl-csharp-generator"))

(in-package :cl-csharp-generator)

;; mkdir source; cd source
;; dotnet --version => 6.0.405
;; dotnet new console --framework net6.0

(progn
  (defparameter *source-dir* #P"example/05_tcp/source/")
  (ensure-directories-exist (asdf:system-relative-pathname
			     'cl-csharp-generator
			     *source-dir*))
  (let ((project "TCPSocketExample"))
    (write-source
     (asdf:system-relative-pathname
      'cl-csharp-generator
      (merge-pathnames
       (format nil "~a.cs" project)
       *source-dir*))
     `(do0
       (using System
	      System.Net
	      System.Net.Sockets
	      System.Text)
       (namespace TCPSocketExample
		  
		  (defclass Program ()
		    (defmethod Main (args)
		      (declare (type "string[]" args)
			       (static))
		      (let ((port 8080)
			    (localAddress IPAddress.Any)
			    (listener (new (TcpListener localAddress
							port))))
			(dot listener
			     (Start))
			(while true
			       (Console.WriteLine
				(string "waiting for connection..."))
			       (let ((client (listener.AcceptTcpClient)))
				 (Console.WriteLine
				  (string "Connected!"))
				 (let ((stream (client.GetStream)))
				  (while true
					 (let ((buffer
						 (new (aref byte
							    (+ (sizeof UInt64)
							       (sizeof Int64)))))
					       (readSize (stream.Read
							  buffer
							  0
							  buffer.Length)))
					   (when (== 0 readSize)
					     break)
					   (let ((timecode (BitConverter.ToUInt64
							    buffer 0))
						 (data (BitConverter.ToInt64
							buffer
							(sizeof UInt64))))
					     (Console.WriteLine
					      (string$ "Timecode={timecode} Data={data}"))
					     ))))))))))))))



