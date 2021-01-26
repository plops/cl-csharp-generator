using Gtk;
using System;
public class GtkTest {
        public static void Main (){
                Application.Init();
                        var win  = new Window("hello mono gtksharp");
        win.Show();
        Application.Run();
}
}