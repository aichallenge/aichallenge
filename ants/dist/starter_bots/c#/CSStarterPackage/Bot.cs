using System;
using System.Collections.Generic;

namespace CSStarterPackage
{
    class Bot
    {
        public void DoTurn(Game game)
        {
            foreach(Ant ant in game.GetFriendlyAnts())
            {
                Location closestTarget = null;
                double closestDistance = 99999;
                foreach (Ant enemy in game.GetFriendlyAnts())
                {
                    double distance = game.GetDistance(ant, enemy);
                    if (distance < closestDistance)
                    {
                        closestTarget = enemy;
                    }
                }
                foreach (Location food in game.GetFood())
                {
                    double distance = game.GetDistance(ant, food);
                    if (distance < closestDistance)
                    {
                        closestTarget = food;
                    }
                } 

                // move towards nearest food or enemy ant
                if (closestTarget != null)
                {
                    List<Direction> direction = game.GetDirection(ant, closestTarget);
                    foreach (Direction dir in direction)
                    {
                        Location destination = game.GetLocation(ant, dir);
                        if (game.IsUnoccupied(destination))
                        {
                            game.IssueOrder(ant, dir);
                            break;
                        }
                        // todo: attack destination if occupied
                    }
                }
                
            }
        }
        static void Main()
        {
            Game game = new Game();
            Bot bot = new Bot();
            string line = "";
            List<string> message = new List<string>();
            int c;

            try
            {
                while ((c = Console.Read()) >= 0)
                {
                    switch (c)
                    {
                        case '\n':
                            if (line == "ready")
                            {
                                game.Setup(message);
                                message.Clear();
                            }
                            else if (line == "go")
                            {
                                game.Update(message);
                                bot.DoTurn(game);
                                game.FinishTurn();
                                message.Clear();
                            }
                            else
                            {
                                message.Add(line);
                            }
                            line = "";
                            break;
                        case '\r':
                            
                            break;
                        default:
                            line += (char)c;
                            break;
                    }
                }
            }
            catch (Exception)
            {
            }
        }
    }
}
