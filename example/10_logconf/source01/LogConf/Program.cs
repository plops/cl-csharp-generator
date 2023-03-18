using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.Logging;
using Microsoft.Extensions.DependencyInjection;
namespace LogConf
{
    public class Program
    {
        public static IServiceProvider BuildServiceProvider()
        {
            var collection = new ServiceCollection();
            // In this C# project, the `ServiceProvider` is used to facilitate dependency injection. Essentially, services are considered dependencies and can be added to the service collection with different lifetimes. 
            // There are three types of lifetimes available: `scoped`, `transient`, and `singleton`. 
            // - `transient` means that a new instance of the service is created every time it is requested from the `ServiceProvider`. 
            // - `scoped` means that the same instance of the service is returned within a specific scope defined by `using (var scope = serviceProvider.CreateScope()) {...}`, but different instances are returned in other scopes. 
            // - `singleton` always returns the same instance of the service. It is important to note that any operations performed by the singleton service must be thread-safe.
            return collection.BuildServiceProvider();
        }
        public static void Main(string[] args)
        {
            Console.WriteLine($"code generation on: 11:09:47 of Saturday, 2023-03-18 (GMT+1)");
            var serviceProvider = BuildServiceProvider();
        }
    }
}