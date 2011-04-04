The files in this package are part of the C++ starter package for Ants from the Google AI Challenge. The Google AI Challenge is an Artificial Intelligence programming contest. You can get more information by visiting www.ai-contest.com.

The entire contents of this starter package are released under the Apache license as is all code related to the Google AI Challenge. See https://github.com/aichallenge/aichallenge/ for more details.

There is a tools package you can download from the contests website, including an engine, visualizer and some sample bots to test against. There are also some tutorials on the website for how you can use them.

How to compile:
The simplest way to compile your bot is to use the Makefile, for example if you are on linux, open up a terminal window, change to the directory containing the sourcefiles and call "make", the produced file "MyBot" ("MyBot.exe" on windows) is what you want to use with the game engine for playing games. You can also call "make clean" to remove the created files.

If you add any additional .cc files to your bot, you will need to alter the Makefile to include them. If you submit your bot with the Makefile included, the server will attempt to use this when compiling your bot, otherwise it will revert back to the auto compile script that is available on the contests github site.

Alternatively, the Makefile is really just making the following calls:
    g++ -O3 -funroll-loops -c Bot.cc -o Bot.o
    g++ -O3 -funroll-loops -c MyBot.cc -o MyBot.o
    g++ -O3 -funroll-loops -c State.cc -o State.o
    g++ -O2 -lm Bot.o MyBot.o State.o -o MyBot

An explanation of the files:
Below is a fairly crude description of what each source file is used for:
    MyBot.cc - This file contains the main function for the bot, it just uses a Bot object to play a single game of Ants.

    Bot.h/cc - The Bot struct represents your bot in the game, the makeMoves function is where you ideally want to write your code for making moves each turn.

    State.h/cc - The State struct stores information about the current state of the game, take a look at the variables and functions for what is already provided, then start adding your own.

    Square.h - The Square struct is used to represent a square on the grid, it contains information like whether the square is water or food, if an ant is there what player it belongs to (this is set to -1 if no ant is currently there) and when it was last seen (this is set to 0 if the square has not been seen).

    Location.h - The Location struct is used to represent a location inside the grid, it simply contains two integers to refer to a row and column.

    Timer.h - The Timer struct allows you to check how long it has been since the start of the turn in milliseconds.

    Bug.h - The Bug struct allows you to easily debug information to files.

For some ideas on improving your bot from here, check out the tutorial and strategy idea pages on the contests website.

