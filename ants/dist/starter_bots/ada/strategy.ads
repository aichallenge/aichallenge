-- Ants Ada Strategy Implementation
-- by philxyz
-- 22nd - 23rd October 2011
-- England, United Kingdom
-- strategy.ads - Ant Logic Package Specification File
-- Customize this file as you see fit but keep Move_Ants and Process_Ant
-- since you probably want to use those :-)

with Ants;

package Strategy is
   -- This procedure "Move_Ants" gets called back to carry out one move.
   -- Your AI logic goes in here. Make use of the functions in the Ants
   -- package to help your code decide which moves to make. For available
   -- helper methods, see the package specification "ants.ads"
   procedure Move_Ants;

   -- Process for moving an individual ant
   procedure Process_Ant (C : Ants.List_Of_Map_Positions.Cursor);
end Strategy;
