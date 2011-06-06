#ifndef SQUARE_H_
#define SQUARE_H_

#include <vector>

/*
    struct for representing a square in the grid.
*/
struct Square
{
    bool isVisible, isWater, isFood;
    int ant;
    std::vector<int> deadAnts;

    Square()
    {
        isVisible = isWater = isFood = 0;
        ant = -1;
    };

    //resets the information for the square except water information
    void reset()
    {
        isVisible = 0;
        isFood = 0;
        ant = -1;
        deadAnts.clear();
    };
};

#endif //SQUARE_H_
