using System;
using System.Net;
using System.Net.Sockets;
using System.Text;
namespace TCPSocketExample {
        public class Program {
                public static void Main (string[] args){
                                    var port  = 8080;
            var localAddress  = IPAddress.Any;
            var listener  = new TcpListener(localAddress, port);
            listener.Start();
            while (true) {
                                Console.WriteLine("waiting for connection...");
                                                var client  = listener.AcceptTcpClient();
                Console.WriteLine("Connected!");
                                var stream  = client.GetStream();
                while (true) {
                                                            var buffer  = new byte[((sizeof(UInt64))+(sizeof(Int64)))];
                    var readSize  = stream.Read(buffer, 0, buffer.Length);
                    if ( (0)==(readSize) ) {
                                                                        break;
}
                                        var timecode  = BitConverter.ToUInt64(buffer, 0);
                    var data  = BitConverter.ToInt64(buffer, sizeof(UInt64));
                    Console.WriteLine($"Timecode={timecode} Data={data}");
}
}
}
}
}