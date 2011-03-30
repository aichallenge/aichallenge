using System;
using System.Collections.Generic;
using System.Text;

namespace CSStarterPackage
{
    class Game
    {
        int rows, cols,
            turn = 0, turns = 0,
            players = 0,
            attackradius2, spawnradius2, viewradius2;

        double loadtime, turntime;
        List<double> scores;
        bool gameover;

        // game map
        char[,] game_map;
        List<Ant> ants = new List<Ant>();
        List<Location> food = new List<Location>();
        List<Location> dead = new List<Location>();
        List<Location> water = new List<Location>();

        // game map data
        const char LAND = '.';
        const char DEAD = 'd';
        const char FOOD = '*';
        const char WATER = '%';
        const char UNSEEN = '?';

        // directions
        private char[] CDIRECTIONS = { 'n', 'e', 's', 'w' };
        private int[,] DIRECTIONS = { { -1, 0 }, { 0, 1 }, { 1, 0 }, { 0, -1 } }; // N, E, S, W

        public Game()
        {
            gameover = false;
        }
        
        public List<Ant> GetAnts()
        {
            return ants;
        }
        /// <summary>
        /// Friendly Ants
        /// </summary>
        /// <returns>list of friendly ants</returns>
        public List<Ant> GetFriendlyAnts()
        {
            List<Ant> friends = new List<Ant>();
            foreach (Ant ant in ants)
            {
                if (ant.owner == 0)
                {
                    friends.Add(ant);
                }
            }
            return friends;
        }
        /// <summary>
        /// Enemy Ants
        /// </summary>
        /// <returns>list of enemy ants</returns>
        public List<Ant> GetEnemyAnts()
        {
            List<Ant> enemy = new List<Ant>();
            foreach (Ant ant in ants)
            {
                if (ant.owner != 0)
                {
                    enemy.Add(ant);
                }
            }
            return enemy;
        }
        /// <summary>
        /// Food items
        /// </summary>
        /// <returns>list of food locations</returns>
        public List<Location> GetFood()
        {
            return food;
        }
        public void Setup(List<string> data)
        {
            try
            {
                foreach (string line in data)
                {
                    string[] tokens = line.ToLower().Split(' ');
                    if (tokens[0] == "cols")
                    {
                        this.cols = int.Parse(tokens[1]);
                    }
                    else if (tokens[0] == "rows")
                    {
                        this.rows = int.Parse(tokens[1]);
                    }
                    else if (tokens[0] == "turns")
                    {
                        this.turns = int.Parse(tokens[1]);
                    }
                    else if (tokens[0] == "loadtime")
                    {
                        this.loadtime = int.Parse(tokens[1]);
                    }
                    else if (tokens[0] == "turntime")
                    {
                        this.turntime = int.Parse(tokens[1]);
                    }
                    else if (tokens[0] == "viewradius2")
                    {
                        this.viewradius2 = int.Parse(tokens[1]);
                    }
                    else if (tokens[0] == "attackradius2")
                    {
                        this.attackradius2 = int.Parse(tokens[1]);
                    }
                    else if (tokens[0] == "spawnradius2")
                    {
                        this.spawnradius2 = int.Parse(tokens[1]);
                    }
                }
                // setup game board
                game_map = new char[rows, cols];
                for (int r = 0; r < rows; r++)
                {
                    for (int c = 0; c < cols; c++)
                    {
                        game_map[r,c] = Game.LAND;
                    }
                }

            }
            catch (Exception)
            {
            }
        }
        public void Update(List<String> data)
        {
            // clear out data from last turn
            this.reset();
            
            // update locations of ants, food, and dead
            foreach (string line in data)
            {
                string[] tokens = line.ToLower().Split(' ');
                if (tokens.Length > 2)
                {
                    int row = int.Parse(tokens[1]);
                    int col = int.Parse(tokens[2]);
                    if (tokens[0] == "w")
                    {
                        this.game_map[row,col] = Game.WATER;
                        this.water.Add(new Location(row, col));
                    }
                    else if (tokens[0] == "a")
                    {
                        int owner = int.Parse(tokens[3]);

                        // players = uppercase, starting at A=0
                        // game world = lowercase
                        this.game_map[row, col] = (char) (owner + 65); 
                        this.ants.Add(new Ant(row, col, owner));
                    }
                    else if (tokens[0] == "f")
                    {
                        this.game_map[row,col] = Game.FOOD;
                        this.food.Add(new Location(row, col));
                    }
                    else if (tokens[0] == "d")
                    {
                        this.game_map[row,col] = Game.DEAD;
                        this.dead.Add(new Location(row, col));
                    }
                }
            }
        }
        private void reset()
        {
            this.ants.Clear();
            this.food.Clear();
            this.dead.Clear();
            this.water.Clear();

            // todo: add unseen '%' code
            for (int row = 0; row < rows; row++)
                for (int col = 0; col < cols; col++)
                    if (game_map[row,col] != '%')
                        game_map[row,col] = '.';
        }
        public void IssueOrder(Ant ant, Direction direction)
        {
            // output: 'o 1 2 n' or move ant at location 1,2 north
            Console.WriteLine("o " + ant.row + " " + ant.col + " " + this.CDIRECTIONS[(int)direction]);
            Console.Out.Flush();
        }
        // returns the eclidean distance between two locations
        public double GetDistance(Location loc1, Location loc2)
        {
            int d1 = Math.Abs(loc1.row - loc2.row),
                d2 = Math.Abs(loc1.col - loc2.col),
                dr = Math.Min(d1, this.rows - d1),
                dc = Math.Min(d2, this.cols - d2);

            return Math.Sqrt(dr * dr + dc * dc);
        }
        public List<Direction> GetDirection(Location loc1, Location loc2)
        {
            List<Direction> directions = new List<Direction>();
            if (loc1.row < loc2.row)
            {
                if (loc2.row - loc1.row >= this.rows / 2)
                {
                    directions.Add(Direction.NORTH);
                }
                else
                {
                    directions.Add(Direction.SOUTH);
                }
            }
            else if (loc1.row > loc2.row)
            {
                if (loc1.row - loc2.row >= this.rows / 2)
                {
                    directions.Add(Direction.SOUTH);
                }
                else
                {
                    directions.Add(Direction.NORTH);
                }
            }

            if (loc1.col < loc2.col)
            {
                if (loc2.col - loc1.col >= this.cols / 2)
                {
                    directions.Add(Direction.WEST);
                }
                else
                {
                    directions.Add(Direction.EAST);
                }
            }
            else if (loc1.col > loc2.col)
            {
                if (loc1.col - loc2.col >= this.cols / 2)
                {
                    directions.Add(Direction.WEST);
                }
                else
                {
                    directions.Add(Direction.EAST);
                }
            }
            return directions;
        }
        public bool IsUnoccupied(Location loc)
        {
            // land, food and dead are unoccupied
            if (this.game_map[loc.row, loc.col] == Game.LAND || this.game_map[loc.row, loc.col] == Game.FOOD || this.game_map[loc.row, loc.col] == Game.DEAD)
            {
                return true;
            }
            // eg Player A to Z, or Game.Water
            return false;
        }
        /// <summary>
        /// Create a new location constrained by the map size by moving in the direction specified
        /// </summary>
        /// <param name="loc">original location</param>
        /// <param name="direction">direction to move</param>
        /// <returns>location created after moving in direction from original location</returns>
        public Location GetLocation(Location loc, Direction direction)
        {
            return new Location((loc.row + this.DIRECTIONS[(int)direction, 0]) % rows, (loc.col + this.DIRECTIONS[(int)direction, 1]) % cols);
        }
        public void FinishTurn()
        {
            Console.WriteLine("go");
            turn++;
            Console.Out.Flush();
        }
        
    }
}
