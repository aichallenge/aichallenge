// NOTE: This was the original starter package but I was asked to
// simplify it.
// 
// You can compile it by simply replacing it with "YourCode.c"

#include "ants.h"

// The following is an example program displaying a basic ai that
// picks a direction for each ant, waits until the ant hits the wall
// and then follows that wall around in a circle. This is not intended
// to be an advanced ai, but just something to show you how to get
// started.
//
// To see all the information contained in struct game_state and
// struct game_info check ants.h. There is also a distance function
// provided in bot.c


// This pointer will point to an array of directions corresponding
// to each of our ants. It will ensure they keep moving in the same
// direction (unless of course blocked by a wall)

char *directions = 0;
int direction_num = 0;

void do_turn(struct game_state *Game, struct game_info *Info) {

    // allocation and reallocation of the directions array

    if (directions == 0) {
        direction_num = 10;
        directions = malloc(direction_num);
        memset(directions, -1, direction_num);
    }
    else if (direction_num < Game->my_ant_index) {
        int old_num = direction_num;
        direction_num *= 2;
        directions = realloc(directions, direction_num); 
        memset(directions + old_num, -1, old_num);
    }

    // shortens the typing a bit and makes the code more clear
    // you don't have to use this

    #define UP -Info->cols
    #define DOWN Info->cols
    #define LEFT -1
    #define RIGHT 1

    int i = 0;
    int j = 0;

    // An array of moves. The ai will start an ant going in a
    // direction based on the letter at moves[BOT_ID % 4]
    // and then cycle through the other moves as required
    // by obstacles on the map.

    char *moves = "WNESWNE";

    for (; i < Game->my_count; ++i) {

        // more abstractions for convenience

        #define ROW Game->my_ants[i].row
        #define COL Game->my_ants[i].col
        #define ID Game->my_ants[i].id

        // now we find the position of our ant

        int offset = ROW*Info->cols + COL;
        char obj_west, obj_east, obj_north, obj_south;

        // We now store the objects north, south, east and west from
        // us in a set of variables. There is a special case in each
        // of these checks under "else" to account for map wrapping 

        if (COL != 0)
            obj_west = Info->map[offset + LEFT];
        else
            obj_west = Info->map[offset + Info->cols - 1];

        if (COL != Info->cols - 1)
            obj_east = Info->map[offset + RIGHT];
        else
            obj_east = Info->map[offset - Info->cols - 1];

        if (ROW != 0)
            obj_north = Info->map[offset + UP];
        else
            obj_north = Info->map[offset + (Info->rows - 1)*Info->cols];

        if (ROW != Info->rows - 1)
            obj_south = Info->map[offset + DOWN];
        else
            obj_south = Info->map[offset - (Info->rows - 1)*Info->cols];

        // we will now cycle through directions and take the first
        // one that works

        int start = Game->my_ants[i].id % 4;
        char dir = 0;
        j = 0;

        for (; j < 4; ++j) {
            char test;
            
            // If we were already going in a direction check that direction
            // from there. Otherwise use the result of ANT_ID % 4

            if (directions[ID] != -1) {
                test = moves[directions[ID] + j];
            }
            else
                test = moves[start + j]; 

            // The "%" symbol represents a blockage. Check the _init_map
            // function in ants.c for more map information.
            //
            // Here we check if the direction we want to take is free.

            switch(test) {
                case 'W':
                    if (obj_west != '%')
                        dir = 'W';
                    break;

                case 'N':
                    if (obj_north != '%')
                        dir = 'N';
                    break;

                case 'E':
                    if (obj_east != '%')
                        dir = 'E';
                    break;

                case 'S':
                    if (obj_south != '%')
                        dir = 'S';
                    break;
            }

            // if we've found a direction, break

            if (dir != 0)
                break;
        }

        // Now we do our move

        if (dir != 0) {
            move(i, dir, Game, Info);
            
            // update the directions array

            j = 0;
            while (moves[j] != dir)
                ++j;

            directions[ID] = j;
        }
    }

    // There are many ways to make this program better.
    // For starters, try to avoid collisions between your
    // own ants and make a conscious effort to gather food
    // instead of walking around at random.
    //
    // Good luck!
}
