-- Ants Ada Implementation
-- by philxyz
-- October 22nd-23rd 2011
-- England, United Kingdom
-- mybot.adb - Program Entry Point

-- Include (at least) the Ants Starter Pack.
with Ants;

-- Program entry point.
procedure MyBot is
begin
   -- The Ants package communicates with the game for you.
   -- all you have to do is implement Strategy.Move_Ants
   Ants.Game_Loop;
end MyBot;
