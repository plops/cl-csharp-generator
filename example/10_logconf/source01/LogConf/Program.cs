using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.Logging;
using Microsoft.Extensions.DependencyInjection;
using CommandLine;
using CommandLine.Text;
namespace LogConf
{
    public interface IConfig
    {
        string? Executable { get; }
        string? LogFile { get; }
        string? DebugLevel { get; }
        string? ConfigFile { get; }
        string? HostName { get; }
        int? Port { get; }
        IConfig MergeObject(IConfig other);
    }
    public class Config : IConfig
    {
        public Config()
        {
            Console.WriteLine($"construct Config");
        }
        [Option('x', "executable", Required = false, HelpText = "bla")]
        public string? Executable { get; set; }
        [Option('f', "log-file", Required = false, HelpText = "bla")]
        public string? LogFile { get; set; }
        [Option('l', "debug-level", Required = false, HelpText = "bla")]
        public string? DebugLevel { get; set; }
        [Option('c', "config-file", Required = false, HelpText = "bla")]
        public string? ConfigFile { get; set; }
        [Option('h', "host-name", Required = false, HelpText = "bla")]
        public string? HostName { get; set; }
        [Option('p', "port", Required = false, HelpText = "bla")]
        public int? Port { get; set; }
        public IConfig MergeObject(IConfig other)
        {
            var result = new Config();
            result.Executable = this.Executable ?? other.Executable;
            result.LogFile = this.LogFile ?? other.LogFile;
            result.DebugLevel = this.DebugLevel ?? other.DebugLevel;
            result.ConfigFile = this.ConfigFile ?? other.ConfigFile;
            result.HostName = this.HostName ?? other.HostName;
            result.Port = this.Port ?? other.Port;
            return result;
        }
    }
    public interface ILogger
    {
        void Log(string message);
    }
    public class Logger : ILogger
    {
        public Logger()
        {
            Console.WriteLine($"construct Logger");
        }
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
            Console.WriteLine($"construct Processor");
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
            var options = Parser.Default.ParseArguments<Config>(args).WithNotParsed((errors) =>
            {
                Console.WriteLine(HelpText.AutoBuild<Config>(null, null));
            }).MapResult((options) =>
            {
                return options;
            }, (errors) =>
            {
                return null;
            });
            Console.WriteLine($" options.Executable='{options.Executable}'");
            Console.WriteLine($" options.LogFile='{options.LogFile}'");
            Console.WriteLine($" options.DebugLevel='{options.DebugLevel}'");
            Console.WriteLine($" options.ConfigFile='{options.ConfigFile}'");
            Console.WriteLine($" options.HostName='{options.HostName}'");
            Console.WriteLine($" options.Port='{options.Port}'");
            var configuration = new ConfigurationBuilder().AddJsonFile(options.ConfigFile, optional: false).Build();
            IConfig config = configuration.Get<Config>();
            Console.WriteLine($" config.Executable='{config.Executable}'");
            Console.WriteLine($" config.LogFile='{config.LogFile}'");
            Console.WriteLine($" config.DebugLevel='{config.DebugLevel}'");
            Console.WriteLine($" config.ConfigFile='{config.ConfigFile}'");
            Console.WriteLine($" config.HostName='{config.HostName}'");
            Console.WriteLine($" config.Port='{config.Port}'");
            var options2 = options.MergeObject(config);
            Console.WriteLine($" options2.Executable='{options2.Executable}'");
            Console.WriteLine($" options2.LogFile='{options2.LogFile}'");
            Console.WriteLine($" options2.DebugLevel='{options2.DebugLevel}'");
            Console.WriteLine($" options2.ConfigFile='{options2.ConfigFile}'");
            Console.WriteLine($" options2.HostName='{options2.HostName}'");
            Console.WriteLine($" options2.Port='{options2.Port}'");
            collection.AddSingleton<IConfig>(config);
            collection.AddTransient<ILogger, Logger>();
            collection.AddSingleton<Processor>();
            return collection.BuildServiceProvider();
        }
        public static void Main(string[] args)
        {
            Console.WriteLine($"code generation on: 22:31:22 of Monday, 2023-03-20 (GMT+1)");
            var serviceProvider = BuildServiceProvider(args);
            var p = serviceProvider.GetService<Processor>();
            p.Process();
        }
    }
}