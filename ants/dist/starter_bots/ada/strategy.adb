-- Ants Ada Strategy Implementation
-- by philxyz
-- 22nd - 23rd October 2011
-- England, United Kingdom
-- strategy.adb - Ant Logic

package body Strategy is
   use type Ants.Bearing;

   -- Values 0 - 3 which wrap around so as to index the "next" bearing to try from the
   -- bearing array. You probably won't need this type declaration in your own code.
   type Bearing_Index is mod 4;

   -- Given an index of type Bearing_Index, returns the corresponding Bearing
   Bearings : array(Bearing_Index'Range) of Ants.Bearing := (Ants.North, Ants.East, Ants.South, Ants.West);

   -- The index into the "Bearings" array of the next "Current_Bearing"
   -- You probably won't need this in your own code.
   Next_Bearing_Index : Bearing_Index := 0;

   -- This procedure "Move_Ants" gets called back to carry out one move.
   -- Your AI logic goes in here. Make use of the functions in the Ants
   -- package to help your code decide which moves to make. For available
   -- helper methods, see the package specification "ants.ads"
   procedure Move_Ants is
      -- Declare P to be a Vector of Map Positions, initializing the list to the result
      -- of a call to the procedure "Ants.My_Ants"
      P: Ants.List_Of_Map_Positions.Vector := Ants.My_Ants;
   begin
      -- For each item in the vector, call back the "Process_Ant" procedure. Process_Ant
      -- is the procedure which makes the decision of where the current ant should
      -- try to move next.
      P.Iterate (Process => Process_Ant'Access);
   end;

   -- This procedure is where the bot should make its decision about where to move and
   -- for you to then call Ants.Issue_Order to request the move to occur.
   procedure Process_Ant (C : Ants.List_Of_Map_Positions.Cursor) is
      My_Position : Ants.Map_Position := Ants.List_Of_Map_Positions.Element (Position => C);
      Sq : Ants.Square := Ants.Examine_Square (My_Position, Bearings (Next_Bearing_Index));
      Dont_Crash : Natural := 0; -- Break out of the loop after trying all 4 bearings without
        -- any success. Should never occur anyway.
   begin
      -- Starter bot logic as described here:
      -- https://github.com/aichallenge/aichallenge/wiki/Ants-Starter-Pack-Guide
      -- (Attempt to go in the following directions in this order: N, E, S, W)

      -- While it is not possible to go in the requested direction, look at the next direction
      -- in the list. 
      while Sq.Kind not in Ants.Passable_Tile or else Sq.Kind not in Ants.Unoccupied_Tile loop
         Dont_Crash := Dont_Crash + 1;
         if Dont_Crash > 4 then
            exit;
         end if;
         Next_Bearing_Index := Next_Bearing_Index + 1;
         Sq := Ants.Examine_Square (My_Position, Bearings (Next_Bearing_Index));
      end loop;

      Ants.Issue_Order (My_Position, Bearings (Next_Bearing_Index));

   end Process_Ant;
end Strategy;
