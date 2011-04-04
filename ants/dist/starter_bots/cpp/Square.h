#ifndef SQUARE_H_
#define SQUARE_H_

#include <vector>

/*
    struct for representing a square in the grid.
*/
struct Square
{
    bool isWater, isFood;
    int ant, lastSeen;
    std::vector<int> deadAnts;

    Square()
    {
        isWater = isFood = 0;
        ant = -1;
        lastSeen = 0;
    };

    //resets the information for the square except water information
    void reset()
    {
        isFood = 0;
        ant = -1;
        deadAnts.clear();
    };
};

#endif //SQUARE_H_
