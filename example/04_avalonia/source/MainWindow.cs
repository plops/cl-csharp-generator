using Avalonia;
using Avalonia.Controls;
using Avalonia.Markup.Xaml;
namespace MyApp {
        public class MainWindow  : Window {
                public  MainWindow (){
                        InitializeComponent();
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