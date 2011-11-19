// Copyright (c) 2011, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

/**
 * A collection of helper io functions implemented using node.js.
 *
 * Idea is to clone the node.js API as closely as possible while adding types.
 * Dart libraries on top of this will experiment with different APIs.
 */
#library('node');
#native('io_node.js');

VM get vm() native;
FS get fs() native;
Path get path() native;
Http get http() native;
Readline get readline() native;

var createSandbox() native
  "return {'require': require, 'process': process, 'console': console};";

typedef void RequestListener(ServerRequest request, ServerResponse response);

class Http native "Http" {
  Server createServer(RequestListener listener) native;
}

class Server native "Server" {
  void listen(int port, [String hostname, Function callback]) native;
}

class ServerRequest native "ServerRequest" {
  final String method;
  final String url;
  final Map<String, String> headers;
  final String httpVersion;

  void setEncoding([String encoding]) {}
}

class ServerResponse native "ServerResponse" {
  int statusCode;

  void setHeader(String name, String value) native;

  String getHeader(String name) native;

  void removeHeader(String name) native;

  void write(String data, [String encoding = 'utf8']) native;

  void end([String data, String encoding = 'utf8']) native;
}

class console native "console" {
  // TODO(jimhug): Map node.js's ability to take multiple args to what?
  static void log(String text) native;
  static void info(String text) native;
  static void warn(String text) native;
  static void error(String text) native;
}

// TODO(nweiz): change this class name to "Process"
class process native "process" {
  static List<String> argv;
  // TODO(nweiz): add Stream type information
  static ReadableStream stdin;
  static WriteableStream stdout;
  static String version;

  static void exit([int code = 0]) native;
  static String cwd() native;
}

class ReadableStream native "ReadableStream" {
  void resume() native;
  void setEncoding(String encoding) native;
  void on(String event, Function listener) native;
  void destroy() native;
}

class WriteableStream native "WriteableStream" {
  int fd;
  void flush() native;
}

class VM native "VM" {
  void runInThisContext(String code, [String filename]) native;
  void runInNewContext(String code, [var sandbox, String filename]) native;
  Script createScript(String code, [String filename]) native;
}

class Script native "Script" {
  void runInThisContext() native;
  void runInNewContext([Map sandbox]) native;
}

class FS native "FS" {
  void writeFileSync(String outfile, String text) native;

  String readFileSync(String filename, [String encoding = 'utf8']) native;

  String realpathSync(String path) native;
  
  void writeSync(int fd, String text) native;
}

class Path native "Path" {
  bool existsSync(String filename) native;
  String dirname(String path) native;
  String basename(String path) native;
  String extname(String path) native;
  String normalize(String path) native;
  // TODO(jimhug): Get the right signatures for normalizeArray and join
}

class Readline native "Readline" {
  ReadlineInterface createInterface(input, output) native;
}

interface ReadlineInterface {
  void setPrompt(String prompt, [int length]);
  void prompt();
  void on(String event, Function callback);
}
