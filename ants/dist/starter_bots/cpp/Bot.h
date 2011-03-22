#ifndef BOT_H_
#define BOT_H_

#include "State.h"

struct Bot
{
    State state;

    Bot();

    void playGame();

    void makeMoves();
    void endTurn();
};

#endif //BOT_H_
