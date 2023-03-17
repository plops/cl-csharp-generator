dotnet new console -n ConsoleApp
dotnet new sln -n ConsoleApp
dotnet sln add ConsoleApp/ConsoleApp.csproj

cd ConsoleApp
dotnet add package Microsoft.Extensions.Configuration
dotnet add package Microsoft.Extensions.Logging
dotnet add package Microsoft.Extensions.Configuration.Json
dotnet add package Microsoft.Extensions.Configuration.CommandLine
