// Copyright 2010 owners of the AI Challenge project
//
// Licensed under the Apache License, Version 2.0 (the "License"); you may not
// use this file except in compliance with the License. You may obtain a copy
// of the License at http://www.apache.org/licenses/LICENSE-2.0 . Unless
// required by applicable law or agreed to in writing, software distributed
// under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
// CONDITIONS OF ANY KIND, either express or implied. See the License for the
// specific language governing permissions and limitations under the License.
//
// Author: Jeff Cameron (jeff@jpcameron.com)
//
// Plays a game of Planet Wars between two computer programs.

public class Engine {
    public static void KillClients(List<Process> clients) {
	for (Process p : clients) {
	    if (p != null) {
		p.Kill();
	    }
	}
    }

    public static boolean AllTrue(List<Boolean> v) {
	for (Boolean b : v) {
	    if (!b.booleanValue()) {
		return false;
	    }
	}
	return true;
    }

    public static void main(String[] args) {
	// Check the command-line arguments.
	if (args.length < 6) {
	    System.err.println("ERROR: you must give at least five command-" +
			       "line arguments.");
	    System.err.println("USAGE: engine map_file_name max_turn_time " +
			       "max_num_turns player_one player_two " +
			       "[more_players]");
	    System.exit(1);
	}
	// Initialize the game. Load the map.
	String mapFilename = args[1];
	int maxNumTurns = Integer.parseInt(args[3]);
	Game game(mapFilename, maxNumTurns);
	if (!game.Init()) {
	    System.err.println("ERROR: failed to start game. map: " +
			       mapFilename);
	}
	long max_turn_time = Integer.parseInt(args[2]);
	// Start the client programs (players).
	List<Process> clients = new ArrayList<Process>();
	for (int i = 4; i < args.length; ++i) {
	    String command = args[i];
	    Process client = Runtime.exec(command);
	    if (!client.Init()) {
		KillClients(clients);
		System.err.println("ERROR: failed to start client: " +
				   command);
		System.exit(1);
	    }
	    clients.add(client);
	}
	// Enter the main game loop.
	while (game.Winner() < 0) {
	    // Send the game state to the clients.
	    System.out.println("The game state:");
	    System.out.print(game);
	    for (int i = 0; i < clients.size(); ++i) {
		if (!clients.get(i).IsAlive() || !game.IsAlive(i + 1)) {
		    continue;
		}
		String message = game.toString(i + 1) + "go\n";
		clients.get(i).getOutputStream().write(message.getBytes());
	    }
	    // Get orders from the clients.
	    long startTime = System.currentTimeMillis();
	    List<Boolean> clientDone = new ArrayList<Boolean>();
	    for (int i = 0; i < clients.size(); ++i) {
		clientDone.add(false);
	    }
	    while (!AllTrue(clientDone) &&
		   (System.currentTimeMillis() - startTime) * 1000 /
		   CLOCKS_PER_SEC <= max_turn_time) {
		for (int i = 0; i < clients.size(); ++i) {
		    Process p = clients.get(i);
		    if (p.IsAlive() && !clientDone[i]) {
			String order;
			int readResult = clients[i]->ReadLine(order);
			if (read_result > 0) {
			    std::cout << "Player " << (i + 1) << ": " << order << std::endl;
			    if (order == "go" || order == "GO" || order == "Go") {
				client_done[i] = true;
			    } else {
				int order_result = game.IssueOrder(i + 1, order);
				if (order_result < 0) {
				    std::cerr << "Killed player " << (i + 1) << " due to "
					      << "error while processing order: " << order
					      << std::endl;
				    clients[i]->Kill();
				}
			    }
			}
		    }
		}
		sleep(0);
	    }
	    // Drop players who didn't respond before the timeout cutoff.
	    for (unsigned int i = 0; i < clients.size(); ++i) {
		if (clients[i]->IsAlive() && !client_done[i]) {
		    std::cerr << "Killing player " << (i + 1) << " for timing out."
			      << std::endl;
		    clients[i]->Kill();
		    game.DropPlayer(i + 1);
		}
	    }
	    game.DoTimeStep();
	}
	KillClients(clients);
	if (game.Winner() > 0) {
	    std::cout << "Player " << game.Winner() << " Wins!" << std::endl;
	} else {
	    std::cout << "Draw!" << std::endl;
	}
	std::cout << "Game playback string: " << game.GamePlaybackString()
		  << std::endl;
	return 0;
    }
}
