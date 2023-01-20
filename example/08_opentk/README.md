Show .NET core code for Linux that creates a Window and draws a circle
with OpenGL.

```
using OpenTK;
using OpenTK.Graphics;
using OpenTK.Graphics.OpenGL;
using OpenTK.Input;

class Game : GameWindow
{
    public Game() : base(800, 600, GraphicsMode.Default, "OpenTK Quickstart Sample")
    {
        VSync = VSyncMode.On;
    }

    protected override void OnLoad(EventArgs e)
    {
        base.OnLoad(e);

        GL.ClearColor(0.1f, 0.2f, 0.5f, 0.0f);
    }

    protected override void OnResize(EventArgs e)
    {
        base.OnResize(e);

        GL.Viewport(0, 0, Width, Height);
    }

    protected override void OnUpdateFrame(FrameEventArgs e)
    {
        base.OnUpdateFrame(e);

        if (Keyboard[Key.Escape])
            Exit();
    }

    protected override void OnRenderFrame(FrameEventArgs e)
    {
        base.OnRenderFrame(e);

        GL.Clear(ClearBufferMask.ColorBufferBit | ClearBufferMask.DepthBufferBit);

        GL.Begin(PrimitiveType.Triangles);
        GL.Color3(1.0f, 0.0f, 0.0f);
        GL.Vertex2(-1.0f, 1.0f);
        GL.Color3(0.0f, 1.0f, 0.0f);
        GL.Vertex2(0.0f, -1.0f);
        GL.Color3(0.0f, 0.0f, 1.0f);
        GL.Vertex2(1.0f, 1.0f);
        GL.End();

        GL.Begin(PrimitiveType.TriangleFan);
        GL.Color3(0.0f, 1.0f, 0.0f);
        GL.Vertex2(0.0f, 0.0f);
        int num_segments = 100;
        double theta = 2 * 3.1415926 / num_segments;
        double c = Math.Cos(theta);//precalculate the sine and cosine
        double s = Math.Sin(theta);
        double t;

        double x = 1;//we start at angle = 0
        double y = 0;

        for (int ii = 0; ii < num_segments; ii++)
        {
            GL.Vertex2(x, y);//output vertex

            //apply the rotation matrix
            t = x;
            x = c * x - s * y;
            y = s * t + c * y;
        }
        GL.End();

        SwapBuffers();
    }

    [STAThread]
    public static void Main()
    {
        using (var game = new Game())
        {
            game.Run(60.0);
        }
    }
}


```




Show .NET core code for Linux that creates a Window with Imgui.Net and
draws a circle. Show a widget that controls the diameter of the
circle.



```
using ImGuiNET;
using OpenTK;
using OpenTK.Graphics.OpenGL;

class Program
{
    private static float _circleDiameter = 100.0f;

    static void Main(string[] args)
    {
        using (var window = new GameWindow())
        {
            // Initialize ImGui
            ImGui.CreateContext();
            var io = ImGui.GetIO();
            io.ConfigFlags |= ImGuiConfigFlags.NavEnableKeyboard;
            ImGui.StyleColorsDark();

            window.Load += (sender, e) =>
            {
                // Set up OpenGL
                GL.ClearColor(0.0f, 0.0f, 0.0f, 1.0f);
                GL.Enable(EnableCap.Blend);
                GL.BlendFunc(BlendingFactorSrc.SrcAlpha, BlendingFactorDest.OneMinusSrcAlpha);
            };

            window.Resize += (sender, e) =>
            {
                io.DisplaySize = new Vector2(window.Width, window.Height);
            };

            window.UpdateFrame += (sender, e) =>
            {
                // Start the ImGui frame
                ImGui.NewFrame();

                // Create the window
                ImGui.Begin("Circle Control");
                ImGui.SliderFloat("Diameter", ref _circleDiameter, 50.0f, 200.0f);
                ImGui.End();

                // Render the ImGui frame
                ImGui.Render();
            };

            window.RenderFrame += (sender, e) =>
            {
                GL.Viewport(0, 0, window.Width, window.Height);
                GL.Clear(ClearBufferMask.ColorBufferBit);

                // Draw the circle
                ImGui.GetBackgroundDrawList().AddCircleFilled(new Vector2(window.Width / 2, window.Height / 2), _circleDiameter / 2, 0xFFFFFFFF);

                // Render the ImGui draw lists
                ImGui.Render();

                window.SwapBuffers();
            };

            window.Run();
        }
    }
}


```