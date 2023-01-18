using Avalonia;
using Avalonia.Controls;
using Avalonia.Markup.Xaml;
namespace {
        public class MainWindow  : Window {
                public void MainWindow (){
                        InitializeComponent();
                        this.AttachDevTools();
}
                private void InitializeComponent (){
                        AvaloniaXamlLoader.Load(this);
}
}
        public class Program {
                public static void Main (string[] args){
                        AppBuilder.Configure<App>().UsePlatformDetect().Start<MainWindow>();
}
}
}