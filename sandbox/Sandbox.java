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

import java.io.*;
import java.util.*;

class Sandbox {
    // Sets up the sandbox, but does not invoke it. This constructor sets up a
    // sandbox with no security at all. The spawned process can do whatever it
    // wants. To actually kick off the command, call the Init() method. If
    // trap_sterr is true, then the sandbox catches the stderr output of the
    // spawned program, otherwise the stderr output of the spawned program goes
    // straight to the terminal.
    public Sandbox(String command, boolean trapStderr) {
	childStdin = null;
	childStdout = null;
	childStderr = null;
	this.command = command;
	process = null;
	this.trapStderr = trapStderr;
	isAlive = false;
    }

    // Sets up the sandbox, but does not invoke it. This constructor sets up a
    // sandbox with no security at all. The spawned process can do whatever it
    // wants. To actually kick off the command, call the Init() method. Traps
    // the stderr channel of the spawned process.
    public Sandbox(String command) {
	childStdin = null;
	childStdout = null;
	childStderr = null;
	this.command = command;
	process = null;
	this.trapStderr = true;
	isAlive = false;	
    }

    // The Init() method actually sets up the sandbox and invokes the command
    // inside it. Returns 1 on success, 0 on failure.
    public int Init() {
	try {
	    process = Runtime.getRuntime().exec(command);
	} catch (Exception e) {
	    return 0;
	}
	if (process == null) {
	    return 0;
	}
	childStdin = new OutputStreamWatcher(process.getOutputStream());
	childStdout = new InputStreamWatcher(process.getInputStream());
	childStderr = new InputStreamWatcher(process.getErrorStream());
	childStdin.start();
	childStdout.start();
	childStderr.start();
	return 1;
    }

    // Destroys the spawned process.
    public void Kill() {
	if (process != null) {
	    process.destroy();
	    childStdin.Kill();
	    childStdout.Kill();
	    childStderr.Kill();
	}
	process = null;
    }

    // Returns true if the spawned process is thought to be alive. If something
    // is known to be wrong with the process, then false is returned. Examples
    // include if an IO function has failed or if the Kill() function has been
    // called.
    public boolean IsAlive() {
	return process != null;
    }

    // Writes the given string to the stdin channel of the spawned process,
    // followed by a single newline character (\n). On success, returns the
    // number of bytes written. On error, -1 is returned.
    public int WriteLine(String message) {
	Write(message);
	childStdin.AddCharacters("\n");
	return message.length() + 1;
    }

    // Writes the given string to the stdin channel of the spawned process,
    // without adding a newline character to the end. On success, returns the
    // number of bytes written. On error, -1 is returned.
    public int Write(String message) {
	childStdin.AddCharacters(message);
	return message.length();
    }

    // Attempts to read a line from the stdout channel of the spawned process.
    // This method does not block.
    //
    // If a complete line of the program's output is available, it is placed
    // into the buf variable and the method returns the number of characters
    // read. The terminating newline character is not included in buf, but is
    // included in the returned character count.
    //
    // If a complete line of the program's output is not yet available, null is
    // returned.
    public String ReadLine() {
	String s = childStdout.Readline();
	return s;
    }

    // A blocking ReadLine. Waits for characters written to the spawned program's
    // stdout stream until a newline is read, or max_blocking_time milliseconds
    // have elapsed. Returns the number of characters read.
    public String ReadLine(int maxBlockingTime) {
	long startTime = System.currentTimeMillis();
	while (System.currentTimeMillis() - startTime < maxBlockingTime) {
	    String s = ReadLine();
	    if (s != null) {
		return s;
	    }
	    try {Thread.sleep(10); } catch (Exception e) { }
	}
	return null;
    }

    // Reads a line from the spawned program's stderr channel. Works just the
    // same as the ReadLine method, except for stderr instead of stdout.
    public String ReadErrorLine() {
	return childStderr.Readline();
    }

    // Returns the shell command used to initialize this sandbox.
    public String Command() {
	return command;
    }

    // Continuously monitors an InputStream in a separate thread.
    class InputStreamWatcher extends Thread {
        public InputStreamWatcher(InputStream inputStream) {
	    this.inputStream = inputStream;
	    buffer = "";
	    lines = new LinkedList<String>();
	    dieDieDie = false;
	}

	public void run() {
	    while (!dieDieDie) {
		if (!Iteration()) {
		    return;
		}
		try {Thread.sleep(10); } catch (Exception e) { }
	    }
	}

	// Kills this thread. Stops monitoring the InputStream.
	public synchronized void Kill() {
	    dieDieDie = true;
	}

	// Checks to get one line of input, if available. If no line is
	// immediately available, returns null.
	public synchronized String Readline() {
	    return lines.poll();
	}

	// Checks the stream. Returns false if there's a problem. If everything
	// is peachy, returns true.
	private synchronized boolean Iteration() {
	    if (inputStream == null) {
		return false;
	    }
	    int c = -1;
	    try {
		int avail = inputStream.available();
		if (avail > 0) {
		    c = inputStream.read();
		}
	    } catch (Exception e) {
		return false;
	    }
	    if (c < 0) {
		return true;
	    }
	    if (c == '\n') {
		lines.offer(buffer);
		buffer = new String("");
	    } else {
		buffer += (char)c;
	    }
	    return true;
	}

	private InputStream inputStream;
	private String buffer;
	private Queue<String> lines;
	private boolean dieDieDie;
    }

    // Continuously monitors an OutputStream in a separate thread.
    class OutputStreamWatcher extends Thread {
        public OutputStreamWatcher(OutputStream inputStream) {
	    this.outputStream = outputStream;
	    buffer = "";
	    dieDieDie = false;
	}

	public void run() {
	    while (!dieDieDie) {
		if (!Iteration()) {
		    return;
		}
		try {Thread.sleep(10); } catch (Exception e) { }
	    }
	}

	// Kills this thread. Stops monitoring the InputStream.
	public synchronized void Kill() {
	    dieDieDie = true;
	}

	// Adds to the queue of characters that are being sent to the sandboxed
	// process' stdin channel.
	public synchronized void AddCharacters(String s) {
	    buffer += s;
	}

	// Writes characters from the buffer to the sandboxed process' stdin
	// channel.
	private synchronized boolean Iteration() {
	    if (outputStream == null) {
		return false;
	    }
	    if (buffer.length() == 0) {
		return true;
	    }
	    try {
		outputStream.write(buffer.getBytes());
		outputStream.flush();
		buffer = new String("");
	    } catch (Exception e) {
		return false;
	    }
	    return true;
	}

	private OutputStream outputStream;
	private String buffer;
	private boolean dieDieDie;
    }

    // Three file descriptors that connect to the stdin, stdout, and stderr
    // streams of the spawned process.
    private OutputStreamWatcher childStdin;
    private InputStreamWatcher childStdout;
    private InputStreamWatcher childStderr;

    // The shell command to be invoked inside this sandbox.
    private String command;

    // The spawned process.
    private Process process;

    // Whether or not to trap the spawned program's stderr output.
    private boolean trapStderr;

    // Keeps track of whether the spawned process is in good shape.
    private boolean isAlive;
}
