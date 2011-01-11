#include <iostream>

int main(int argc, char *argv[]) {
  int c;
  std::string line;
  while ((c = std::cin.get()) != EOF) {
    if (c == '\n') {
      if (line == "go") {
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
