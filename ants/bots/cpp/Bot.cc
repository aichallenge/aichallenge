#include "Bot.h"

Bot::Bot()
{

};

//plays a single game of Ants.
void Bot::playGame()
{
    //reads the game parameters
    cin >> state;

    //continues making moves while the game is not over
    while(cin >> state)
    {
        makeMoves();
        endTurn();
    }
};

//makes the bots moves for the turn
void Bot::makeMoves()
{
    state.bug << "turn " << state.turn << ":" << endl;
    state.bug << state << endl;

    //picks out moves for each ant
    for(int ant=0; ant<(int)state.ants.size(); ant++)
    {
        for(int d=0; d<TDIRECTIONS; d++)
        {
            Location loc = state.getLocation(state.ants[ant], d);

            if(state.grid[loc.row][loc.col] != '%')
            {
                state.makeMove(state.ants[ant], d);
                break;
            }
        }
    }

    state.bug << "time taken: " << state.timer.getTime() << "ms" << endl << endl;
};

//finishes the turn
void Bot::endTurn()
{
    state.reset();
    state.turn++;

    cout << "go" << endl;
};
