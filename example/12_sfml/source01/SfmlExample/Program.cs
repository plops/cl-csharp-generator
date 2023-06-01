// <copyright file="Program.cs" company="Plops">
// Copyleft (c) 2023 Plops. This file is MIT Licensed.
// </copyright>
namespace SfmlExample
{
    using SFML.Graphics;
    using SFML.System;
    using SFML.Window;

    public class Program
    {
        public static void Main(string[] args)
        {
            Console.WriteLine($"code generation on: 21:39:52 of Thursday, 2023-06-01 (GMT+1)");
            var window = new RenderWindow(new VideoMode(800, 600), "SFML.Net Window");
            var circle = new CircleShape(50f);
            circle.FillColor = Color.Red;
            circle.Position = new Vector2f(50f, 50f);
            var clock = new Clock();
            var baseRadius = 50f;
            var pulseSpeed = (float)((((2) * (Math.PI))) / ((1.30f)));
            while (window.IsOpen)
            {
                window.DispatchEvents();
                window.Clear();
                var time = clock.ElapsedTime.AsSeconds();
                circle.Radius = ((baseRadius) * (((((1) + ((float)Math.Sin(((pulseSpeed) * (time)))))) / (2))));
                window.Draw(circle);
                window.Display();
            }
        }
    }
}