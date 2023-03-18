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
            return collection.BuildServiceProvider();
        }
        public static void Main(string[] args)
        {
            Console.WriteLine($"code generation on: 10:47:57 of Saturday, 2023-03-18 (GMT+1)");
        }
    }
}