// Copyleft (c) 2023 Plops; MIT License
using SFML.Graphics;
using SFML.Window;
using SFML.System;
namespace SfmlExample {
        public class Program {
                public static void Main (string[] args){
                                    Console.WriteLine($"code generation on: 20:45:02 of Thursday, 2023-06-01 (GMT+1)");
                        var window  = new RenderWindow(new VideoMode(800, 600), "SFML.Net Window");
            var circle  = new CircleShape(50f);
                        circle.FillColor=Color.Red;
            circle.Position=new Vector2f(50f, 50f);
            while (window.IsOpen) {
                                window.DispatchEvents();
                                window.Clear();
                                window.Draw(circle);
                                window.Display();
}
}
}
}