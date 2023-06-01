using System;
using SFML.Graphics;
using SFML.Window;
using SFML.System;

namespace SFMLNetExample
{
    class Program
    {
        static void Main(string[] args)
        {
            // Create the main window
            var window = new RenderWindow(new VideoMode(800, 600), "SFML.Net Window");

            // Create a circle
            var circle = new CircleShape(50f)
            {
                FillColor = Color.Red,
                Position = new Vector2f(50f, 50f)
            };

            // Start the game loop
            while ((window.IsOpen))
            {
                // Process events
                window.DispatchEvents();

                // Clear screen
                window.Clear();

                // Draw the circle
                window.Draw(circle);

                // Update the window
                window.Display();
            }
        }
    }
}
