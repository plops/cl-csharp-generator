using System;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.Http;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using Grpc.Core;
using Grpc.AspNetCore;
using MyService;
namespace Server {
        public class Program {
                public static void Main (string[] args){
                        CreateHostBuilder(args).Build().Run();
}
                public static IHostBuilder CreateHostBuilder (string[] args){
                                    var hostBuilder  = Host.CreateDefaultBuilder(args);
            hostBuilder.ConfigureWebHostDefaults(( webBuilder)=> {
                                webBuilder.UseStartup<Startup>();
});
            return hostBuilder;
}
}
        public class Startup {
                public void ConfigureServices (IServiceCollection services){
                        services.AddGrpc();
}
                public void Configure (IApplicationBuilder app, IWebHostEnvironment env){
                        if ( env.IsDevelopment() ) {
                                app.UseDeveloperExceptionPage();
}
                        app.UseRouting();
                        app.UseEndpoints(( endpoints)=> {
                                endpoints.MapGrpcService<MyServiceImpl>().EnableGrpcWeb();
});
}
}
        public class MyServiceImpl  : MyService.MyService.MyServiceBase {
                public Task<HelloReply> SayHello (HelloRequest request, ServerCallContext context){
                        return Task.FromResult(new HelloReply);
}
}
}