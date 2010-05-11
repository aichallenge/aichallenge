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

#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <vector>
#include "cpp_util/string_util.h"
#include "planet_wars/engine/game.h"
#include "sandbox/sandbox.h"

void KillClients(std::vector<Sandbox*>& clients) {
  for (unsigned int i = 0; i < clients.size(); ++i) {
    if (clients[i] != NULL) {
      clients[i]->Kill();
      delete clients[i];
    }
    clients[i] = NULL;
  }
}

bool AllTrue(const std::vector<bool>& v) {
  for (unsigned int i = 0; i < v.size(); ++i) {
    if (!v[i]) {
      return false;
    }
  }
  return true;
}

int main(int argc, char *argv[]) {
  // Check the command-line arguments.
  if (argc < 6) {
    std::cerr << "ERROR: you must give at least five command-line arguments"
	      << std::endl << "USAGE: engine map_file_name max_turn_time "
	      << "max_num_turns player_one player_two [more_players]"
	      << std::endl;
    exit(1);
  }
  // Initialize the game. Load the map.
  std::string map_file_name = std::string(argv[1]);
  int max_num_turns = atoi(argv[3]);
  Game game(map_file_name, max_num_turns);
  if (!game.Init()) {
    std::cerr << "ERROR: failed to start game. map: "
	      << map_file_name << std::endl;
  }
  long max_turn_time = atol(argv[2]);
  // Start the client programs (players).
  std::vector<Sandbox*> clients;
  for (int i = 4; i < argc; ++i) {
    std::string command(argv[i]);
    Sandbox *client = new Sandbox(command);
    if (!client->Init()) {
      KillClients(clients);
      std::cerr << "ERROR: failed to start client: " << command << std::endl;
      exit(1);
    }
    clients.push_back(client);
    std::cout << "Successfully invoked " << command
	      << " pid: " << getpid() << std::endl;
  }
  // Enter the main game loop.
  while (game.Winner() < 0) {
    // Send the game state to the clients.
    std::string game_state_string = game.ToString();
    std::cout << "The game state: " << std::endl << game_state_string;
    for (unsigned int i = 0; i < clients.size(); ++i) {
      if (!clients[i]->IsAlive() || !game.IsAlive(i + 1)) {
	continue;
      }
      int result = clients[i]->WriteLine(game.ToString(i + 1) + "go");
      if (result < 0) {
	std::cerr << "WARNING: failed to communicate with client: "
		  << clients[i]->Command() << std::endl;
	clients[i]->Kill();
      }
    }
    // Get orders from the clients.
    time_t start_time = clock();
    std::vector<bool> client_done(clients.size(), false);
    while (!AllTrue(client_done) &&
	   (clock() - start_time) * 1000 / CLOCKS_PER_SEC <= max_turn_time) {
      for (unsigned int i = 0; i < clients.size(); ++i) {
	if (clients[i]->IsAlive() && !client_done[i]) {
	  std::string order;
	  int read_result = clients[i]->ReadLine(order);
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
