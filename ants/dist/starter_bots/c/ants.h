#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

// this header is basically self-documenting

struct game_info {
	int loadtime;
	int turntime;
	int rows;
	int cols;
	int turns;
	int viewradius_sq;
	int attackradius_sq;
	int spawnradius_sq;
    int seed;
	char *map;
};

struct basic_ant {
    int row;
    int col;
    char player;
};

struct my_ant {
    int id;
    int row;
    int col;
};

struct food {
    int row;
    int col;
};

struct game_state {
    struct my_ant *my_ants;
    struct basic_ant *enemy_ants;
    struct food *food;
    struct basic_ant *dead_ants;
    
    int my_count;
    int enemy_count;
    int food_count;
    int dead_count;

    int my_ant_index;
};
