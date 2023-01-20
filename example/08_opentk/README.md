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