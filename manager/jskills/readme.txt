TSUpdate is a very simple command line utility for calculating updated player
trueskill information after a multiplayer, single player on a team game. It
receives the information on standard input and then outputs the updated player
information to standard out.

To compile it use the command "javac -cp JSkills_0.9.0.jar:. TSUpdate.java"

To run use the command "java -cp JSkills_0.9.9.jar:. TSUpdate"

It expects each player's information from the game on a line in the following
format:
P <player id> <game rank> <prior mu> <prior sigma>

player id, can be any string not containing whitespace and must be unique to
each player in the game.

game rank, is the final ranking of the player in the game and must be an
integer, but does not need to have any other structure other than lesser ranks
are considered to have come in ahead of greater ranks. Players with the same rank are considered to have tied.

prior mu and prior sigma, are floating point representations of the player's
trueskill stats before the game.

After all players are entered the program expects one line with "C" and will
then calculate and output each player's updated trueskill stats with:
<player id> <updated mu> <updated sigma>

The program will then exit.

