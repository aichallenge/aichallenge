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
  trap_stderr_ = trap_stderr;
  is_alive_ = false;
  child_stdout_thread_ = -1;
  child_stderr_thread_ = -1;
  child_stdout_buffer_length_ = 0;
  child_stderr_buffer_length_ = 0;
}

Sandbox::~Sandbox() {
  Kill();
}

int Sandbox::Init() {
  // Tokenize the program arguments.
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
  // Set up pipes to communicate with the client.
  int child_stdin_pipe[2];
  int child_stdout_pipe[2];
  int child_stderr_pipe[2];
  if (pipe(child_stdin_pipe) ||
      pipe(child_stdout_pipe) ||
      pipe(child_stderr_pipe)) {
    delete argv;
    return 0;
  }
  // Exec the client process.
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
    exit(1); // We should never get here because execv shouldn't return.
  }
  // Set up the pipes to be non-blocking.
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
  // Start up some threads to continuously monitor the client's stdout and
  // stderr streams and buffer any output in memory.
  child_stdout_thread_ = fork();
  if (child_stdout_thread_ == -1) {
    Kill();
    return 0;
  } else if (child_stdout_thread_ == 0) {
    ChildStdoutMonitor();
  }
  if (trap_stderr_) {
    child_stderr_thread_ = fork();
    if (child_stderr_thread_ == -1) {
      Kill();
      return 0;
    } else if (child_stderr_thread_ == 0) {
      ChildStderrMonitor();
    }
  }
  is_alive_ = true;
  return 1;
}

void Sandbox::Kill() {
  is_alive_ = false;
  if (pid_ == 0) {
    return;
  }
  kill(pid_, SIGTERM);
  kill(child_stdout_thread_, SIGTERM);
  kill(child_stderr_thread_, SIGTERM);
  int child_pid = fork();
  if (child_pid == -1) {
    return;
  } else if (child_pid == 0) {
    sleep(1);
    kill(pid_, SIGKILL);
    kill(child_stdout_thread_, SIGKILL);
    kill(child_stderr_thread_, SIGKILL);
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

void Sandbox::SlideCharactersBack(char *s, int start, int end) {
  for (int i = 0; i < end - start; ++i) {
    s[i] = s[start + i];
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
  std::string line;
  for (int i = 0; i < child_stderr_buffer_length_; ++i) {
    char c = child_stderr_buffer_[--child_stderr_buffer_length_];
    if (c == '\n') {
      buf = line;
      SlideCharactersBack(child_stderr_buffer_,
			  i + 1,
			  child_stderr_buffer_length_);
      child_stderr_buffer_length_ -= i;
      return i + 1;
    } else {
      line += c;
    }
  }
  return 0;
}

int Sandbox::getcpid() {
  return pid_;
}

int Sandbox::ReadLine(std::string& buf) {
  std::string line;
  //std::cout << child_stdout_buffer_length_;
  for (int i = 0; i < child_stdout_buffer_length_; ++i) {
    char c = child_stdout_buffer_[--child_stdout_buffer_length_];
    std::cout << "popping character: " << c << std::endl;
    if (c == '\n') {
      buf = line;
      SlideCharactersBack(child_stdout_buffer_,
			  i + 1,
			  child_stdout_buffer_length_);
      child_stdout_buffer_length_ -= i;
      return i + 1;
    } else {
      line += c;
    }
  }
  return 0;
}

void Sandbox::ChildStdoutMonitor() {
  char c;
  while (true) {
    //std::cout << child_stdout_buffer_length_;
    ssize_t bytes_read = read(child_stdout_, &c, 1);
    if (bytes_read > 0) {
      std::cout << "pushing character: " << c << std::endl;
      child_stdout_buffer_[child_stdout_buffer_length_++] = c;
    }
    sched_yield();
  }
}

void Sandbox::ChildStderrMonitor() {
  char c;
  while (true) {
    ssize_t bytes_read = read(child_stderr_, &c, 1);
    if (bytes_read > 0) {
      child_stderr_buffer_[child_stderr_buffer_length_++] = c;
    }
    sched_yield();
  }
}
