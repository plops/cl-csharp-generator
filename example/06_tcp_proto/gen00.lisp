(eval-when (:compile-toplevel :execute :load-toplevel)
     (ql:quickload "cl-csharp-generator"))

(in-package :cl-csharp-generator)

;; mkdir source; cd source
;; dotnet --version => 6.0.405
;; dotnet new console --framework net6.0

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
	      System.Net
	      System.Net.Sockets
	      System.IO
	      ProtoBuf)
       (namespace TCPProtoBufExample
		  (defclass ProtoBufPacket ()
		    (declare (public))
		    (bracket-n ProtoContract)
		    (defclass Packet ()
		      (declare (public))
		      (bracket-n (ProtoMember 1))
		      (space-n public UInt64 Timecode (progn get set))
		      (bracket-n (ProtoMember 2))
		      (space-n public Int64 DataValue (progn get set))
		      ))
		  (defclass Program ()
		    (defmethod Main (args)
		      (declare (type "string[]" args)
			       (static))
		      (let ((listener (new (TcpListener IPAddress.Any 1234))))
			
			(dot listener
			     (Start))
			(while true
			       (Console.WriteLine
				(string "waiting for connection..."))
			       (let ((client (listener.AcceptTcpClient)))
				 (Console.WriteLine
				  (string "Connected!"))
				 (let ((stream (client.GetStream))
				       (packet (Serializer.Deserialize<ProtoBufPacket.Packet> stream)))
				   (Console.WriteLine
					      (string$ "Timecode={packet.Timecode} Data={packet.DataValue}"))
				   )))))))))))



