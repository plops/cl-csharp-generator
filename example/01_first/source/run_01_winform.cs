using System;
using System.Drawing;
using System.Windows.Forms;
public class HelloWorld  : Form {
        public static void Main (){
                Application.Run(new HelloWorld());
}
        public  HelloWorld (){
                        var b  = new Button();
                b.Text="Click Me!";
        (b.Click)+=(new EventHandler(ButtonClick));
}
        private void ButtonClick (object sender, EventArgs e){
                MessageBox.Show("Button Clicked!");
}
}