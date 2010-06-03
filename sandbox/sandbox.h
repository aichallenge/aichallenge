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
// The sandbox is a wrapper that can be used to invoke untrusted code securely.

#ifndef SANDBOX_SANDBOX_H_
#define SANDBOX_SANDBOX_H_

#define MAX_BUFFER_LENGTH 1024

#include <pthread.h>
#include <string>

class Sandbox {
 public:
  // Sets up the sandbox, but does not invoke it. This constructor sets up a
  // sandbox with no security at all. The spawned process can do whatever it
  // wants. To actually kick off the command, call the Init() method. If
  // trap_sterr is true, then the sandbox catches the stderr output of the
  // spawned program, otherwise the stderr output of the spawned program goes
  // straight to the terminal.
  Sandbox(const std::string& command, bool trap_stderr = true);

  // Destructor. Destroys the spawned process.
  ~Sandbox();

  // The Init() method actually sets up the sandbox and invokes the command
  // inside it. Returns 1 on success, 0 on failure.
  int Init();

  // Destroys the spawned process.
  void Kill();

  // Returns true if the spawned process is thought to be alive. If something
  // is known to be wrong with the process, then false is returned. Examples
  // include if an IO function has failed or if the Kill() function has been
  // called.
  bool IsAlive();

  // Writes the given string to the stdin channel of the spawned process,
  // followed by a single newline character (\n). On success, returns the
  // number of bytes written. On error, -1 is returned.
  int WriteLine(const std::string& message);

  // Writes the given string to the stdin channel of the spawned process,
  // without adding a newline character to the end. On success, returns the
  // number of bytes written. On error, -1 is returned.
  int Write(const std::string& message);

  // Attempts to read a line from the stdout channel of the spawned process.
  // This method does not block.
  //
  // If a complete line of the program's output is available, it is placed
  // into the buf variable and the method returns the number of characters
  // read. The terminating newline character is not included in buf, but is
  // included in the returned character count.
  //
  // If a complete line of the program's output is not yet available, the
  // method returns zero, and the length of buf will be zero. If an error
  // occurs, -1 is returned.
  //
  // How can you tell the difference between a blank line being read, and no
  // line being available? If the first case, the return value is nonzero. In
  // the second case, the return value is zero.
  int ReadLine(std::string& buf);

  // A blocking ReadLine. Waits for characters written to the spawned program's
  // stdout stream until a newline is read, or max_blocking_time milliseconds
  // have elapsed. Returns the number of characters read.
  int ReadLine(std::string& buf, int max_blocking_time);

  // Reads a line from the spawned program's stderr channel. Works just the
  // same as the ReadLine method, except for stderr instead of stdout.
  int ReadErrorLine(std::string& buf);

  // Returns the shell command used to initialize this sandbox.
  std::string Command();

  // Returns the pid of the spawned child
  int getcpid();

  // These methods continuously monitor the child process' stdout and stderr
  // streams, buffering the output as it arrives.
  void ChildStdoutMonitor();
  void ChildStderrMonitor();

 private:
  // Sets a file descriptor to eb non-blocking.
  int SetNonBlockingIO(int file_descriptor);

  // Deletes the first n characters of a C-style character array by sliding
  // the following characters to the left.
  void SlideCharactersBack(char *s, int start, int end);

  // Three file descriptors that connect to the stdin, stdout, and stderr
  // streams of the spawned process.
  int child_stdin_;
  int child_stdout_;
  int child_stderr_;

  // These are two processes that are spawned to continuously monitor the
  // client's stdout and stderr stream for new output. Any output found is
  // added to the relevant buffers.
  pthread_t child_stdout_thread_;
  pthread_t child_stderr_thread_;

  // The shell command to be invoked inside this sandbox.
  std::string command_;

  // The process ID of the spawned process.
  int pid_;

  // Buffers that store characters that have been read from the spawned
  // process' stdout and stderr channels.
  std::string child_stdout_buffer_;
  std::string child_stderr_buffer_;

  // Whether or not to trap the spawned program's stderr output.
  bool trap_stderr_;

  // Keeps track of whether the spawned process is in good shape.
  bool is_alive_;

  // If this value is true, all threads associated with the sandbox should
  // cease executing.
  bool diediedie_;
};

#endif
