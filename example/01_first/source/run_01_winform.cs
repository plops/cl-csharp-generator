using System;
using System.Drawing;
using System.Windows.Forms;
class(HelloWorld, Form(), declare(public ), public void Main (){
        Application.Run(new HelloWorld());
}, void HelloWorld (){
            var b  = new Button();
        b.Text="Click Me!";
    (b.Click)+=(new EventHandler(ButtonClick));
}, private void ButtonClick (object sender, EventArgs e){
        MessageBox.Show("Button Clicked!");
});