using System;
using System.IO;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.Logging;

class Program
{
    static void Main(string[] args)
    {
        var config = new ConfigurationBuilder()
            .AddJsonFile("appsettings.json", optional: true, reloadOnChange: true)
            .AddCommandLine(args)
            .Build();

        var executable = config.GetValue<string>("x");
        var logfile = config.GetValue<string>("f");
        var configfile = config.GetValue<string>("c");
        var debuglevel = config.GetValue<string>("l");

        if (!string.IsNullOrEmpty(configfile))
        {
            var configFile = new ConfigurationBuilder()
                .AddJsonFile(configfile, optional: true, reloadOnChange: true)
                .Build();

            executable = configFile.GetValue<string>("Executable") ?? executable;
            logfile = configFile.GetValue<string>("LogFile") ?? logfile;
            debuglevel = configFile.GetValue<string>("DebugLevel") ?? debuglevel;
        }

        var loggerFactory = LoggerFactory.Create(builder =>
        {
            builder.AddConsole();
            builder.SetMinimumLevel(Enum.Parse<LogLevel>(debuglevel ?? "Information"));
        });

        ILogger<Program> logger =

loggerFactory.CreateLogger<Program>();

        logger.LogInformation($"Executable: {executable}");
        logger.LogInformation($"Log File: {logfile}");
        logger.LogInformation($"Config File: {configfile}");
        logger.LogInformation($"Debug Level: {debuglevel}");

        // Rest of the program logic goes here
    }
}