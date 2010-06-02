#include <iostream>
#include "sandbox/sandbox.h"

int main(int argc, char *argv[]) {
  Sandbox sandbox("./sum");
  std::cout << "init: " << sandbox.Init() << std::endl;
  sandbox.WriteLine("1 2 3");
  std::string response;
  std::cout << "status: " << sandbox.ReadLine(response, 1000) << std::endl;
  std::cout << "response: " << response << std::endl;
  return 0;
}
