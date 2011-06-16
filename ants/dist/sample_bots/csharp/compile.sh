SRC=../../starter_bots/csharp
gmcs /debug -out:GreedyBot.exe -pkg:dotnet $SRC/Ants.cs $SRC/GameState.cs $SRC/Location.cs $SRC/Tile.cs $SRC/Bot.cs GreedyBot.cs
