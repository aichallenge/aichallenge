#include <iostream>
#include "sandbox/sandbox.h"

int main(int argc, char *argv[]) {
  Sandbox sandbox("./sum");
  std::cout << "init: " << sandbox.Init() << std::endl;
  sandbox.WriteLine("1 2 3");
  sandbox.WriteLine("4 5 6");
  sandbox.WriteLine("7 8 9");
  std::string response;
  while (sandbox.ReadLine(response, 1000) > 0) {
    std::cout << "response: " << response << std::endl;
  }
  return 0;
}
