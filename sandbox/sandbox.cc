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

#include "sandbox/sandbox.h"
#include <fcntl.h>
#include <iostream>
#include <sched.h>
#include <signal.h>
#include <stdlib.h>
#include <string>
#include <ctime>
#include <vector>
#include "cpp_util/string_util.h"

Sandbox::Sandbox(const std::string& command, bool trap_stderr) {
  child_stdin_ = -1;
  child_stdout_ = -1;
  child_stderr_ = -1;
  command_ = command;
  pid_ = 0;
  child_stdout_buffer_ = "";
  child_stderr_buffer_ = "";
  trap_stderr_ = trap_stderr;
  is_alive_ = false;
}

Sandbox::~Sandbox() {
  Kill();
}

int Sandbox::Init() {
  std::vector<std::string> tokens;
  StringUtil::Tokenize(command_, " ", tokens);
  char **argv = new char*[tokens.size()+1];
  if (argv == NULL) {
    return 0;
  }
  for (unsigned int i = 0; i < tokens.size(); ++i) {
    argv[i] = (char*)tokens[i].c_str();
  }
  argv[tokens.size()] = NULL;
  int child_stdin_pipe[2];
  int child_stdout_pipe[2];
  int child_stderr_pipe[2];
  if (pipe(child_stdin_pipe) ||
      pipe(child_stdout_pipe) ||
      pipe(child_stderr_pipe)) {
    delete argv;
    return 0;
  }
  pid_ = fork();
  if (pid_ == -1) {
    delete argv;
    return 0;
  } else if (pid_ == 0) {
    dup2(child_stdin_pipe[0], 0);
    dup2(child_stdout_pipe[1], 1);
    if (trap_stderr_) {
      dup2(child_stderr_pipe[1], 2);
    }
    execv(argv[0], argv);
    exit(1);
  } else {
    child_stdin_ = child_stdin_pipe[1];
    child_stdout_ = child_stdout_pipe[0];
    if (trap_stderr_) {
      child_stderr_ = child_stderr_pipe[0];
    } else {
      child_stderr_ = -1;
    }
    SetNonBlockingIO(child_stdin_);
    SetNonBlockingIO(child_stdout_);
    if (trap_stderr_) {
      SetNonBlockingIO(child_stderr_);
    }
    delete argv;
    is_alive_ = true;
    return 1;
  }
}

void Sandbox::Kill() {
  is_alive_ = false;
  if (pid_ == 0) {
    return;
  }
  kill(pid_, SIGTERM);
  int child_pid = fork();
  if (child_pid == -1) {
    return;
  } else if (child_pid == 0) {
    sleep(1);
    kill(pid_, SIGKILL);
    exit(0);
  }
}

std::string Sandbox::Command() {
  return command_;
}

bool Sandbox::IsAlive() {
  return is_alive_;
}

int Sandbox::SetNonBlockingIO(int fd)
{
  int flags;
#if defined(O_NONBLOCK) // If they have O_NONBLOCK, use the Posix way to do it.
  if (-1 == (flags = fcntl(fd, F_GETFL, 0))) {
    flags = 0;
  }
  return fcntl(fd, F_SETFL, flags | O_NONBLOCK);
#else // Otherwise, use the old way of doing it.
  flags = 1;
  return ioctl(fd, FIOBIO, &flags);
#endif
}

int Sandbox::WriteLine(const std::string& message) {
  std::string with_newline = message + "\n";
  return write(child_stdin_, with_newline.c_str(), with_newline.length());
}

int Sandbox::Write(const std::string& message) {
  return write(child_stdin_, message.c_str(), message.length());
}

int Sandbox::ReadLine(std::string& buf) {
  char c;
  while (true) {
    ssize_t bytes_read = read(child_stdout_, &c, 1);
    switch (bytes_read) {
    case 1:
      if (c == '\n') {
	buf = std::string(child_stdout_buffer_);
	child_stdout_buffer_ = std::string("");
	return buf.length() + 1;
      } else {
	child_stdout_buffer_ += c;
      }
      break;
    case 0:
      return 0;
    case -1:
      return 0;
    }
  }
}

int Sandbox::ReadLine(std::string& buf, int max_blocking_time) {
  clock_t start_time = clock();
  while (true) {
    int result = ReadLine(buf);
    if (result > 0) {
      return result;
    }
    clock_t current_time = clock();
    int elapsed_ms = (current_time - start_time) * 1000 / CLOCKS_PER_SEC;
    if (elapsed_ms >= max_blocking_time) {
      break;
    }
    sched_yield();
  }
  return 0;
}

int Sandbox::ReadErrorLine(std::string& buf) {
  char c;
  while (true) {
    ssize_t bytes_read = read(child_stderr_, &c, 1);
    switch (bytes_read) {
    case 1:
      if (c == '\n') {
	buf = std::string(child_stderr_buffer_);
	child_stderr_buffer_ = std::string("");
	return buf.length() + 1;
      } else {
	child_stderr_buffer_ += c;
      }
      break;
    case 0:
      return 0;
    case -1:
      return 0;
    }
  } 
  return 0;
}

int Sandbox::getcpid() {
  return pid_;
}
