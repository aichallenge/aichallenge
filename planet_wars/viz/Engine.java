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

import java.io.*;
import java.util.*;

public class Engine {
    public static void KillClients(List<Process> clients) {
	for (Process p : clients) {
	    if (p != null) {
		p.destroy();
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
	if (args.length < 5) {
	    System.err.println("ERROR: you must give at least five command-" +
			       "line arguments.");
	    System.err.println("USAGE: engine map_file_name max_turn_time " +
			       "max_num_turns player_one player_two " +
			       "[more_players]");
	    System.exit(1);
	}
	// Initialize the game. Load the map.
	String mapFilename = args[0];
	int maxNumTurns = Integer.parseInt(args[2]);
	Game game = new Game(mapFilename, maxNumTurns, 0);
	if (game.Init() == 0) {
	    System.err.println("ERROR: failed to start game. map: " +
			       mapFilename);
	}
	long max_turn_time = Integer.parseInt(args[1]);
	// Start the client programs (players).
	List<Process> clients = new ArrayList<Process>();
	for (int i = 3; i < args.length; ++i) {
	    String command = args[i];
	    Process client = null;
	    try {
		client = Runtime.getRuntime().exec(command);
	    } catch (Exception e) {
		client = null;
	    }
	    if (client == null) {
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
	    System.err.println("The game state:");
	    System.err.print(game);
	    for (int i = 0; i < clients.size(); ++i) {
		if (clients.get(i) == null || !game.IsAlive(i + 1)) {
		    continue;
		}
		String message = game.PovRepresentation(i + 1) + "go\n";
		try {
		    OutputStream out = clients.get(i).getOutputStream();
		    OutputStreamWriter writer = new OutputStreamWriter(out);
		    writer.write(message, 0, message.length());
		    writer.flush();
		    
		} catch (Exception e) {
		    clients.set(i, null);
		}
	    }
	    // Get orders from the clients.
	    for (int i = 0 ; i < clients.size(); ++i) {
		InputStream inputStream = clients.get(i).getInputStream();
		InputStreamReader reader = new InputStreamReader(inputStream);
		BufferedReader bufferedReader = new BufferedReader(reader);
		String line;
		boolean done = false;
		try {
		    while (!done && (line = bufferedReader.readLine()) != null) {
			System.err.println("received: " + line);
			line = line.toLowerCase().trim();
			if (line.equals("go")) {
			    done = true;
			} else {
			    game.IssueOrder(i + 1, line);
			}
		    }
		} catch (Exception e) {
		    System.err.println("Problem while getting orders: " + e);
		}
	    }
	    game.DoTimeStep();
	}
	KillClients(clients);
	if (game.Winner() > 0) {
	    System.err.println("Player " + game.Winner() + " Wins!");
	} else {
	    System.err.println("Draw!");
	}
	System.out.println(game.GamePlaybackString());
    }
}
