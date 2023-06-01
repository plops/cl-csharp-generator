
- create the code
```
dotnet new console
dotnet add package SFML.Net --version 2.5.0
dotnet add package StyleCop.Analyzers
pacman -Ss csfml
```

- run stylecop

```
dotnet build --no-incremental 
```

- how to use stylecop without putting it as a dependency into a repo:
https://bigfont.ca/install-and-use-stylecop-in-a-net-or-net-core-project-from-the-command-line/


```
dotnet format --severity=info
```
- i can't figure out how to fix the warnings 
- here is some docs on it but it's not working for me:
https://roundwide.com/dotnet-format-and-stylecop-analyzers/
