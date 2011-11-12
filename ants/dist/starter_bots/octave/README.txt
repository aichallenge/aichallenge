The files in this package are part of the Octave starter package for Ants from the Google AI Challenge. The Google AI Challenge is an Artificial Intelligence programming contest. You can get more information by visiting www.ai-contest.com.

The entire contents of this starter package are released under the Apache license as is all code related to the Google AI Challenge. See https://github.com/aichallenge/aichallenge/ for more details.

There is a tools package you can download from the contests website, including an engine, visualizer and some sample bots to test against. There are also some tutorials on the website for how you can use them.

How to compile:
There's no need to compile! The only line required to run the bot (using the tools) and assuming that octave is installed in the path varible (for windows) is "octave -qf path_to_file/MyBot.m"

An explanation of the files:
Below is a fairly crude description of what each source file is used for:
    MyBot.m - Contains the main function that plays the game as well as the function to decide which moves to make and the function that ends the turn and prints 'go' to the server
    
    state_functions.m - The "juice" of the bot. There's everything from the function that parses the input and saves everything in a struct state, to functions that compute distances between locations, update the information about visible squares, print the moves to the game server, reset the state, etc.

REMARK: All the locations are stored 0-based in the state structure. In order to index the the state.grid you'll obviously have to add 1 to the row and column that are stored elsewhere (you can see examples of this in the code)

For some ideas on improving your bot from here, check out the tutorial and strategy idea pages on the contest website.

Author:
Pedro Osório - 2011
mebm.pedrosorio in the google mail 

