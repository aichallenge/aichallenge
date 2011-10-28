-- Ants Ada Implementation
-- by philxyz
-- October 22nd-23rd 2011
-- England, United Kingdom
-- ants.adb - Library Package Implementation

-- This project is under Apache License v2.0

-- Do not compile with Adacore GNAT GPL and then distribute the binary.
-- since the code would also have to be distributed and to do so would require
-- making the code GPL3 which is incompatible with Apache License required
-- by the Ants project.

with Ada.Containers.Vectors;
with Ada.Numerics.Generic_Elementary_Functions;

package Ants is

   -- Type Declarations
   type Tile is (WATER, FOOD, ANT, LAND, HILL, DEAD);
   subtype Passable_Tile is Tile range FOOD .. DEAD;
   subtype Unoccupied_Tile is Passable_Tile range LAND .. DEAD;
   subtype Occupied_Tile is Tile range WATER .. ANT;

   type Player is new Integer;

   type Bearing is (North, East, South, West);

   type Square is record
      Kind : Tile := Land;
      Owner : Player := -1; -- No owner
      Discovered : Boolean := False; -- Not yet discovered
      Last_Bearing : Bearing := North;
   end record;

   type Map_Dimension is new Natural;

   type Map_Position is record
      Row, Column : Map_Dimension;
   end record;

   type Map is array(Map_Dimension range <>, Map_Dimension range <>) of Square;

   type Map_Pointer is access Map;

   package List_Of_Map_Positions is
     new Ada.Containers.Vectors(Index_Type   => Natural,
                                Element_Type => Map_Position);
   use List_Of_Map_Positions;

   type Bot_Input_Line is new String(1 .. 300);

   package Bot_Input_Cache is
     new Ada.Containers.Vectors(Index_Type   => Positive,
                                Element_Type => Bot_Input_Line);

   type Milliseconds is new Natural;

   type Game_Variables is record
      loadtime, turntime : Milliseconds := 0;

      rows, cols : Map_Dimension := 0;

      turns, viewradius2, attackradius2,
      spawnradius2: Natural := 0;

      player_seed : Long_Integer := 0;
   end record;

   package Float_Math is new Ada.Numerics.Generic_Elementary_Functions(Float);
   use Float_Math;

   -- The active state of the game is a Game_Variables record.
   State : Game_Variables;


   -- CORE...

   -- Read and cache bot input until a 'ready', 'go', or 'end' is received,
   -- then call the update function. The Update, Issue Order and Finish Turn
   -- functions can be rolled into the game loop if clearly commented, unless
   -- the game loop is part of the starter bot logic.
   procedure Game_Loop;

   -- Instruct an ant at the specified square to move in the direction
   -- of the supplied bearing.
   procedure Issue_Order (Pos : Map_Position; B : Bearing);

   -- Function to send a 'go' string to the game engine
   procedure Finish_Turn;


   -- HELPER FUNCTIONALITY...

   -- Function to examine the contents of a square
   function Examine_Square (My_Position : Map_Position; New_Direction : Bearing) return Square;

   -- Function to return an iterable list of hills that belong to the bot.
   function My_Hills return List_Of_Map_Positions.Vector;

   -- Function to return an iterable list of hills that do not belong to
   -- the bot.
   function Enemy_Hills return List_Of_Map_Positions.Vector;

   -- Function to return an iterable list of ants that belong to the bot.
   function My_Ants return List_Of_Map_Positions.Vector;

   -- Function to return an iterable list of ants that do not belong
   -- to the bot.
   function Enemy_Ants return List_Of_Map_Positions.Vector;

   -- Function to return an iterable list of food on the map.
   function Food return List_Of_Map_Positions.Vector;

   -- Provide a function to determine the shortest distance between 2 points
   -- on a torus.
   function Distance (A, B : Map_Position) return Float;

   -- Provide a function to determine the 1 or 2 orthogonal directions from one
   -- point to another on a torus.
   function Direction (A, B : Map_Position) return String;

   -- Provide a function to determine if a square can be seen by the bot.
   function Visible (P : Map_Position) return Boolean;

   -- Update: Provide a function to determine if an ant could be ordered into a
   -- square if vacant.
   function Passable (P : Map_Position) return Boolean;

   -- Update: Provide a function to determine if an ant can be ordered into a
   -- square successfully this turn.
   function Unoccupied (P : Map_Position) return Boolean;

   -- Provide a function to return the number of milliseconds left since the
   -- last 'go' was received from the game engine.
   --   function Time_Remaining return Milliseconds;

private

   -- Bot input cache
   Input_Cache : Bot_Input_Cache.Vector;

   -- Map - we store the map on the heap since we don't know about its
   -- dimensions until we receive them from the game instance.
   Game_Map_Pointer : Map_Pointer;

   -- Parse the bot input and update the internal state of the game.
   -- Should clear ants and food before each turn and only add back what is
   -- given by the engine.
   procedure Update;

   -- Clear positions of Ants and Food ready for a new turn
   procedure Clear_Ants_And_Food;

end Ants;
