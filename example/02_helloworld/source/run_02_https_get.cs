using System;
using System.Net;
public class HttpsTest {
        public static void Main (){
                        var c  = new System.Net.WebClient();
        Console.WriteLine(c.DownloadString("https://www.nuget.org"));
}
}