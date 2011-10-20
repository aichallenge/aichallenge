#ifndef SQUARE_H_
#define SQUARE_H_

#include <vector>

/*
    struct for representing a square in the grid.
*/
struct Square
{
    bool isVisible, isWater, isHill, isFood;
    int ant, hillPlayer;
    std::vector<int> deadAnts;

    Square()
    {
        isVisible = isWater = isHill = isFood = 0;
        ant = hillPlayer = -1;
    };

    //resets the information for the square except water information
    void reset()
    {
        isVisible = 0;
        isHill = 0;
        isFood = 0;
        ant = hillPlayer = -1;
        deadAnts.clear();
    };
};

#endif //SQUARE_H_
