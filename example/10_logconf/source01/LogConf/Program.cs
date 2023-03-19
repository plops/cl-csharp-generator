using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.Logging;
using Microsoft.Extensions.DependencyInjection;
namespace LogConf
{
    public interface IConfig
    {
        string Executable { get; }
        string LogFile { get; }
        string DebugLevel { get; }
        string ConfigFile { get; }
        bool Help { get; }
    }
    public class Config : IConfig
    {
        public string Executable { get; set; }
        public string LogFile { get; set; }
        public string DebugLevel { get; set; }
        public string ConfigFile { get; set; }
        public bool Help { get; set; }
    }
    public interface ILogger
    {
        void Log(string message);
    }
    public class Logger : ILogger
    {
        public void Log(string message)
        {
            Console.WriteLine($"[{DateTime.Now.ToShortTimeString()}] {message}");
        }
    }
    public class Processor
    {
        private readonly IConfig _config;
        private readonly ILogger _logger;
        public Processor(IConfig config, ILogger logger)
        {
            _config = config;
            _logger = logger;
        }
        public void Process()
        {
            Console.WriteLine($" _config.Executable='{_config.Executable}'");
            _logger.Log($"DebugLevel={_config.DebugLevel}");
        }
    }
    public class Program
    {
        public static IServiceProvider BuildServiceProvider(string[] args)
        {
            var collection = new ServiceCollection();
            // In this C# project, the `ServiceProvider` is used to facilitate dependency injection. Essentially, services are considered dependencies and can be added to the service collection with different lifetimes. The `ServiceProvider` will look at the available constructors on the thing you are requesting.
            // There are three types of lifetimes available: `scoped`, `transient`, and `singleton`. 
            // - `transient` means that a new instance of the service is created every time it is requested from the `ServiceProvider`. 
            // - `scoped` means that the same instance of the service is returned within a specific scope defined by `using (var scope = serviceProvider.CreateScope()) {...}`, but different instances are returned in other scopes. 
            // - `singleton` always returns the same instance of the service. It is important to note that any operations performed by the singleton service must be thread-safe.
            var switchMappings = new Dictionary<string, string>() { { "-x", "Executable" }, { "-f", "LogFile" }, { "-l", "DebugLevel" }, { "-c", "ConfigFile" }, { "-h", "Help" } };
            var configuration = new ConfigurationBuilder().AddJsonFile("appSettings.json", optional: false).AddCommandLine(args, switchMappings).Build();
            IConfig config = configuration.Get<Config>();
            collection.AddSingleton<IConfig>(config);
            collection.AddTransient<ILogger, Logger>();
            collection.AddSingleton<Processor>();
            return collection.BuildServiceProvider();
        }
        public static void Main(string[] args)
        {
            Console.WriteLine($"code generation on: 12:59:56 of Sunday, 2023-03-19 (GMT+1)");
            var serviceProvider = BuildServiceProvider(args);
            var p = serviceProvider.GetService<Processor>();
            p.Process();
        }
    }
}