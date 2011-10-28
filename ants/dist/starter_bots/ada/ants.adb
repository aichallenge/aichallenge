-- Ants Ada Implementation
-- by philxyz
-- October 22nd-23rd 2011
-- England, United Kingdom
-- ants.adb - Library Package Implementation

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Latin_1;
with GNAT.String_Split;
with Strategy;

package body Ants is
   procedure Game_Loop is
      Current_Line : Bot_Input_Line;
      Input_Line_Length : Natural;
   begin
      Actual_Game_Loop:
      while not Ada.Text_IO.End_Of_File loop
         Current_Line := (others => Ada.Characters.Latin_1.Space);
         Ada.Text_IO.Get_Line (String (Current_Line), Input_Line_Length);
         Input_Cache.Append (New_Item => Current_Line);

         if Current_Line(1 .. 2) = "go" or else
           Current_Line(1 .. 3) = "end" or else
           Current_Line(1 .. 5) = "ready" then
            Update;
         end if;
      end loop Actual_Game_Loop;
   end Game_Loop;

   procedure Update is
      type Mode is (Game_Config, Turn, Players);

      Current_Mode : Mode := Game_Config;
      Num_Players : Positive := 1;

      procedure Process_Cache_Line (C : Bot_Input_Cache.Cursor) is
         Line : Bot_Input_Line := Bot_Input_Cache.Element (Position => C);
         Subs : GNAT.String_Split.Slice_Set;
         Separators : constant String := " ";
      begin
         GNAT.String_Split.Create (S          => Subs,
                                   From       => String (Line),
                                   Separators => Separators,
                                   Mode       => GNAT.String_Split.Multiple);

         if GNAT.String_Split.Slice (Subs, 1) = "players" then
            Current_Mode := Players;
            Num_Players := Positive'Value (GNAT.String_Split.Slice (Subs, 2));
         elsif GNAT.String_Split.Slice (Subs, 1) = "turn" and then GNAT.String_Split.Slice (Subs, 2) /= "0" then
            Current_Mode := Turn;
            -- Clear ants and food before each new turn
            Clear_Ants_And_Food;
         else -- We are not switching mode, so continue with existing mode
            if Current_Mode = Game_Config then
               if GNAT.String_Split.Slice (Subs, 1) = "loadtime" then
                  State.loadtime := Milliseconds'Value (GNAT.String_Split.Slice (Subs, 2));
               elsif GNAT.String_Split.Slice (Subs, 1) = "turntime" then
                  State.turntime := Milliseconds'Value (GNAT.String_Split.Slice (Subs, 2));
               elsif GNAT.String_Split.Slice (Subs, 1) = "rows" then
                  State.rows := Map_Dimension'Value (GNAT.String_Split.Slice (Subs, 2));
               elsif GNAT.String_Split.Slice (Subs, 1) = "cols" then
                  State.cols := Map_Dimension'Value (GNAT.String_Split.Slice (Subs, 2));
               elsif GNAT.String_Split.Slice (Subs, 1) = "turns" then
                  State.turns := Natural'Value (GNAT.String_Split.Slice (Subs, 2));
               elsif GNAT.String_Split.Slice (Subs, 1) = "viewradius2" then
                  State.viewradius2 := Natural'Value (GNAT.String_Split.Slice (Subs, 2));
               elsif GNAT.String_Split.Slice (Subs, 1) = "attackradius2" then
                  State.attackradius2 := Natural'Value (GNAT.String_Split.Slice (Subs, 2));
               elsif GNAT.String_Split.Slice (Subs, 1) = "spawnradius2" then
                  State.spawnradius2 := Natural'Value (GNAT.String_Split.Slice (Subs, 2));
               elsif GNAT.String_Split.Slice (Subs, 1) = "player_seed" then
                  State.player_seed := Long_Integer'Value (GNAT.String_Split.Slice (Subs, 2));
               end if;
            elsif Current_Mode = Turn then
               -- Read information about visible items
               if GNAT.String_Split.Slice (Subs, 1) = "f" then
                  -- Food Square
                  declare
                     P : Map_Position;
                  begin
                     P.Row := Map_Dimension'Value (GNAT.String_Split.Slice (Subs, 2));
                     P.Column := Map_Dimension'Value (GNAT.String_Split.Slice (Subs, 3));

                     Game_Map_Pointer.all(P.Row, P.Column).Kind := FOOD;
                     Game_Map_Pointer.all(P.Row, P.Column).Discovered := True;
                  end;
               elsif GNAT.String_Split.Slice (Subs, 1) = "w" then
                  -- Water Square
                  declare
                     P : Map_Position;
                  begin
                     P.Row := Map_Dimension'Value (GNAT.String_Split.Slice (Subs, 2));
                     P.Column := Map_Dimension'Value (GNAT.String_Split.Slice (Subs, 3));

                     Game_Map_Pointer.all(P.Row, P.Column).Kind := WATER;
                     Game_Map_Pointer.all(P.Row, P.Column).Discovered := True;
                  end;
               elsif GNAT.String_Split.Slice (Subs, 1) = "a" then
                  -- Live Ant Square
                  declare
                     P : Map_Position;
                  begin
                     P.Row := Map_Dimension'Value (GNAT.String_Split.Slice (Subs, 2));
                     P.Column := Map_Dimension'Value (GNAT.String_Split.Slice (Subs, 3));

                     Game_Map_Pointer.all(P.Row, P.Column).Kind := ANT;
                     Game_Map_Pointer.all(P.Row, P.Column).Owner :=
                       Player'Value (GNAT.String_Split.Slice (Subs, 4));
                     Game_Map_Pointer.all(P.Row, P.Column).Discovered := True;
                  end;
               elsif GNAT.String_Split.Slice (Subs, 1) = "h" then
                  -- Hill Square
                  declare
                     P : Map_Position;
                  begin
                     P.Row := Map_Dimension'Value (GNAT.String_Split.Slice (Subs, 2));
                     P.Column := Map_Dimension'Value (GNAT.String_Split.Slice (Subs, 3));

                     Game_Map_Pointer.all(P.Row, P.Column).Kind := HILL;
                     Game_Map_Pointer.all(P.Row, P.Column).Owner :=
                       Player'Value (GNAT.String_Split.Slice (Subs, 4));
                     Game_Map_Pointer.all(P.Row, P.Column).Discovered := True;
                  end;
               elsif GNAT.String_Split.Slice (Subs, 1) = "d" then
                  -- Dead Ant Square
                  declare
                     P : Map_Position;
                  begin
                     P.Row := Map_Dimension'Value (GNAT.String_Split.Slice (Subs, 2));
                     P.Column := Map_Dimension'Value (GNAT.String_Split.Slice (Subs, 3));

                     Game_Map_Pointer.all(P.Row, P.Column).Kind := Dead;
                     Game_Map_Pointer.all(P.Row, P.Column).Owner :=
                       Player'Value (GNAT.String_Split.Slice (Subs, 4));
                     Game_Map_Pointer.all(P.Row, P.Column).Discovered := True;
                  end;
               end if;
            elsif Current_Mode = Players then
--               if GNAT.String_Split.Slice (Subs, 1) = "score" then
--                  declare
--                     Scores : array(Num_Players) of Natural;
--                  begin

               null;
--                  end;
--               end if;
            end if;
         end if;
      end;
   begin

      -- Iterate over the commands in the Input Cache
      Input_Cache.Iterate (Process => Process_Cache_Line'Access);
      Input_Cache.Clear;

      -- End of game configuration, allocate the playing area/map
      if Current_Mode = Game_Config then
         -- Create a new map of the size specified in the state.
         Game_Map_Pointer := new Map(0 .. State.rows, 0 .. State.cols);

         -- Initialize every square in the matrix to type Tile'Land.
         Game_Map_Pointer.all := (others => (others => (Kind => LAND, Owner => -1, Discovered => False, Last_Bearing => North)));
      end if;

      Strategy.Move_Ants;

      -- Signal that we have processed the commands and are ready to continue
      Finish_Turn;
   end Update;

   procedure Issue_Order (Pos : Map_Position; B : Bearing) is
      BC : Character := 'N'; -- Default to North
   begin
      if B = East then
         BC := 'E';
      elsif B = South then
         BC := 'S';
      elsif B = West then
         BC := 'W';
      end if;

      Ada.Text_IO.Put_Line ("o " & Map_Dimension'Image (Pos.Row) & ' '
                            & Map_Dimension'Image (Pos.Column) & ' ' & BC);
   end Issue_Order;

   procedure Finish_Turn is
   begin
      Ada.Text_IO.Put_Line("go");
   end Finish_Turn;

   function Examine_Square (My_Position : Map_Position; New_Direction : Bearing) return Square is
   begin
      if New_Direction = North then
         if My_Position.Row = 0 then
            return Game_Map_Pointer.all(State.rows - 1, My_Position.Column);
         else
            return Game_Map_Pointer.all(My_Position.Row - 1, My_Position.Column);
         end if;
      elsif New_Direction = East then
         if My_Position.Column = State.cols - 1 then
            return Game_Map_Pointer.all(My_Position.Row, 0);
         else
            return Game_Map_Pointer.all(My_Position.Row, My_Position.Column + 1);
         end if;
      elsif New_Direction = South then
         if My_Position.Row = State.rows - 1 then
            return Game_Map_Pointer.all(0, My_Position.Column);
         else
            return Game_Map_Pointer.all(My_Position.Row + 1, My_Position.Column);
         end if;
      elsif New_Direction = West then
         if My_Position.Column = 0 then
            return Game_Map_Pointer.all(My_Position.Row, State.cols - 1);
         else
            return Game_Map_Pointer.all(My_Position.Row, My_Position.Column - 1);
         end if;
      else
         -- Code should NEVER get here.
         return Game_Map_Pointer.all(My_Position.Row, My_Position.Column);
      end if;
   end Examine_Square;

   function My_Hills return List_Of_Map_Positions.Vector is
      V : List_Of_Map_Positions.Vector;
   begin
      for row in 0 .. State.rows - 1 loop
         for col in 0 .. State.cols -1 loop
            if Game_Map_Pointer.all(row, col).Kind = HILL and then
               Game_Map_Pointer.all(row, col).Owner = 0 then
               declare
                  MP : Map_Position;
               begin
                  MP.Row := row;
                  MP.Column := col;
                  V.Append (New_Item => MP);
               end;
            end if;
         end loop;
      end loop;
      return V;
   end My_Hills;

   function Enemy_Hills return List_Of_Map_Positions.Vector is
      V : List_Of_Map_Positions.Vector;
   begin
      for row in 0 .. State.rows - 1 loop
         for col in 0 .. State.cols -1 loop
            if Game_Map_Pointer.all(row, col).Kind = HILL and then
               Game_Map_Pointer.all(row, col).Owner > 0 then
               declare
                  MP : Map_Position;
               begin
                  MP.Row := row;
                  MP.Column := col;
                  V.Append (New_Item => MP);
               end;
            end if;
         end loop;
      end loop;
      return V;
   end Enemy_Hills;

   function My_Ants return List_Of_Map_Positions.Vector is
      V : List_Of_Map_Positions.Vector;
   begin
      for row in 0 .. State.rows - 1 loop
         for col in 0 .. State.cols -1 loop
            if Game_Map_Pointer.all(row, col).Kind = ANT and then
               Game_Map_Pointer.all(row, col).Owner = 0 then
               declare
                  MP : Map_Position;
               begin
                  MP.Row := row;
                  MP.Column := col;
                  V.Append (New_Item => MP);
               end;
            end if;
         end loop;
      end loop;
      return V;
   end My_Ants;

   function Enemy_Ants return List_Of_Map_Positions.Vector is
      V : List_Of_Map_Positions.Vector;
   begin
      for row in 0 .. State.rows - 1 loop
         for col in 0 .. State.cols -1 loop
            if Game_Map_Pointer.all(row, col).Kind = ANT and then
               Game_Map_Pointer.all(row, col).Owner > 0 then
               declare
                  MP : Map_Position;
               begin
                  MP.Row := row;
                  MP.Column := col;
                  V.Append (New_Item => MP);
               end;
            end if;
         end loop;
      end loop;
      return V;
   end Enemy_Ants;

   function Food return List_Of_Map_Positions.Vector is
      V : List_Of_Map_Positions.Vector;
   begin
      for row in 0 .. State.rows - 1 loop
         for col in 0 .. State.cols -1 loop
            if Game_Map_Pointer.all(row, col).Kind = FOOD then
               declare
                  MP : Map_Position;
               begin
                  MP.Row := row;
                  MP.Column := col;
                  V.Append (New_Item => MP);
               end;
            end if;
         end loop;
      end loop;
      return V;
   end Food;

   function Distance (A, B : Map_Position) return Float is
      row1 : Float := Float (A.Row);
      col1 : Float := Float (A.Column);
      row2 : Float := Float (B.Row);
      col2 : Float := Float (B.Column);

      function Minimum (A, B : Float) return Float is
      begin
         if A <= B then
            return A;
         else
            return B;
         end if;
      end Minimum;
   begin
      return Minimum (abs (col1 - col2), Float (State.cols) - abs (col1 - col2))
        + Minimum (abs (row1 - row2), Float (State.rows) - abs (row1 - row2));
   end Distance;

   function Direction (A, B : Map_Position) return String is
      Dir : String(1 .. 2) := (others => Ada.Characters.Latin_1.NUL);
      row1 : Integer := Integer (A.Row mod State.rows);
      col1 : Integer := Integer (A.Column mod State.cols);
      row2 : Integer := Integer (B.Row mod State.rows);
      col2 : Integer := Integer (B.Column mod State.cols);

      procedure Append (B : Character) is
         Loc : Natural range 0 .. 2 := 0;
      begin
         for c in Dir'Range loop
            if Dir (c) = Ada.Characters.Latin_1.NUL then
               Dir (Loc) := B;
               exit;
            end if;
            Loc := Loc + 1;
         end loop;
      end Append;
   begin
      if row1 < row2 then
         if row2 - row1 >= Integer (State.rows / 2) then
            Append ('N');
         elsif row2 - row1 <= Integer (State.rows / 2) then
            Append ('S');
         end if;
      elsif row2 < row1 then
         if row1 - row2 >= Integer (State.rows / 2) then
            Append ('S');
         elsif row1 - row2 <= Integer (State.rows / 2) then
            Append ('N');
         end if;
      elsif col1 < col2 then
         if col2 - col1 >= Integer (State.cols / 2) then
            Append ('W');
         elsif col2 - col1 <= Integer (State.cols / 2) then
            Append ('E');
         end if;
      elsif col2 < col1 then
         if col1 - col2 >= Integer (State.cols / 2) then
            Append ('E');
         elsif col1 - col2 <= Integer (State.cols / 2) then
            Append ('W');
         end if;
      end if;
      return Dir;
   end Direction;

   function Visible (P : Map_Position) return Boolean is
   begin
      return Game_Map_Pointer.all (P.Row, P.Column).Discovered;
   end Visible;

   function Passable (P : Map_Position) return Boolean is
   begin
      return Game_Map_Pointer.all (P.Row, P.Column).Kind in Passable_Tile;
   end Passable;

   function Unoccupied (P : Map_Position) return Boolean is
      S : Square := Game_Map_Pointer.all (P.Row, P.Column);
   begin
      return S.Kind in Unoccupied_Tile or else S.Discovered = False;
   end Unoccupied;

--   function Time_Remaining return Milliseconds is
   --begin
   --   null;
--end Time_Remaining;

   procedure Clear_Ants_And_Food is
   begin
      for row in 0 .. State.rows - 1 loop
         for col in 0 .. State.cols - 1 loop
            if Game_Map_Pointer.all(row, col).Kind = Ant or else
             Game_Map_Pointer.all(row, col).Kind = Food then
               Game_Map_Pointer.all(row, col).Kind := Land;
            end if;
         end loop;
      end loop;
   end Clear_Ants_And_Food;
end;
