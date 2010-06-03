#include <iostream>
#include "sandbox/sandbox.h"

void ReadLines(Sandbox& sandbox) {
  int status = 1;
  while (status > 0) {
    std::string response;
    status = sandbox.ReadLine(response, 1000);
    std::cout << "status: " << status << std::endl;
    std::cout << "response: " << response << std::endl;
  }
}

int main(int argc, char *argv[]) {
  Sandbox sandbox("/usr/bin/python ../sadbox/sadbox.py -d "
		  "../submissions/122742/ -c /usr/bin/python+MyBot.py");
  //Sandbox sandbox("../../sandbox/sum");
  std::cout << "init: " << sandbox.Init() << std::endl;
  //sandbox.WriteLine("P 0 0 1 30 2");
  //ReadLines(sandbox);
  //sandbox.WriteLine("P 2 4 2 30 2");
  //ReadLines(sandbox);
  //sandbox.WriteLine("P 1 2 0 0 5");
  //ReadLines(sandbox);
  sandbox.WriteLine("go");
  //std::string message = "P 0 0 1 30 2\nP 2 4 2 30 2\nP 1 2 0 0 5\ngo\n";
  //std::cout << "length: " << message.length() << std::endl;
  //std::cout << "write: " << sandbox.Write(message) << std::endl;
  ReadLines(sandbox);
  sandbox.Kill();
  return 0;
}
