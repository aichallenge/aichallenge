#include <iostream>
#include "sandbox/sandbox.h"

int main(int argc, char *argv[]) {
  Sandbox sandbox("/usr/bin/python ../sadbox/sadbox.py -d ../submissions/122742/ -c /usr/bin/python+MyBot.py");
  std::cout << "init: " << sandbox.Init() << std::endl;
  sandbox.WriteLine("P 0 0 1 30 2");
  //sandbox.WriteLine("P 2 4 2 30 2");
  //sandbox.WriteLine("P 1 2 0 0 5");
  sandbox.WriteLine("go");
  int status = 1;
  while (status > 0) {
    std::string response;
    status = sandbox.ReadLine(response, 1000);
    std::cout << "status: " << status << std::endl;
    std::cout << "response: " << response << std::endl;
  }
  sandbox.Kill();
  return 0;
}
