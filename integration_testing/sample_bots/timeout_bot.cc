#include <iostream>
#include <unistd.h>

int main(int argc, char *argv[]) {
  int c;
  int turn = 0;
  std::string line;
  while ((c = std::cin.get()) != EOF) {
    if (c == '\n') {
      if (line == "go") {
	++turn;
	if (turn >= 3) {
	  sleep(5);
	}
	std::cout << "go" << std::endl;
	std::cout.flush();
      }
      line = std::string("");
    } else {
      line += (char)c;
    }
  }
  return 0;
}
