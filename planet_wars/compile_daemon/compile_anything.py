# compile_anything.py
# Author: Jeff Cameron (jeff@jpcameron.com)
#
# Auto-detects the language of the entry in the current working directory
# and attempts to compile it. The auto-detection works by looking for the
# "main" code file of several different languages. If no languages are found
# it is an error, and a message is printed on stderr. If more than one
# language is auto-detected, it is an error, and a message is printed on
# stderr.
#
# To add support for a new language, you need to add a function like
# compile_java, except for the new language. You also need to "register"
# this new function in the programming_languages dictionary.

import os
import sys
import re
import glob
import subprocess
import errno

SAFEPATH = re.compile('[a-zA-Z0-9_.$-]+$')

def safeglob(pattern):
  safepaths = []
  paths = glob.glob(pattern)
  for path in paths:
    if SAFEPATH.match(path):
      safepaths.append(path)
  return safepaths

def nukeglob(pattern):
  paths = glob.glob(pattern)
  for path in paths:
    try:
      os.unlink(path)
    except OSError, e:
      if e.errno != errno.ENOENT:
        raise

def system(args):
  cmd = ' '.join(args) + "\n"
  proc = subprocess.Popen(args, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
  (out, err) = proc.communicate()
  return cmd + out, err

def check_path(path):
  if not os.path.exists(path):
    return "Failure: output file " + str(path) + " was not created\n"
  else:
    return ""

# This it the routine that is invoked to compile entries written in Java
def compile_java():
  out_message = ""
  err_message = ""
  nukeglob('*.class')
  nukeglob('*.jar')
  sources = safeglob('*.java')
  out, err = system(['javac'] + sources)
  out_message += out
  err_message += err
  out, err = system(['jar', 'cfe', 'MyBot.jar', 'MyBot'] + \
    safeglob('*.class'))
  out_message += out
  err_message += err
  err_message += check_path('MyBot.jar')
  os.chmod("MyBot.jar", 0644)
  return out_message, err_message

# This it the routine that is invoked to compile entries written in Haskell
def compile_haskell():
  nukeglob('MyBot')
  out, err = system(['ghc', '--make', 'MyBot.hs', '-O2', '-v0'])
  err += check_path('MyBot')
  return out, err

# This is the routine that is invoked to compile entires written in C#
def compile_cs():
  nukeglob('MyBot.exe')
  sources = safeglob('*.cs')
  out, err = system(['gmcs', '-warn:0', '-out:MyBot.exe'] + sources)
  err += check_path('MyBot.exe')
  return out, err

# This it the routine that is invoked to compile entries written in C++
def compile_cpp():
  out_message = ""
  err_message = ""
  nukeglob('*.o')
  nukeglob('MyBot')
  sources = safeglob('*.c') + safeglob('*.cc') + safeglob('*.cpp')
  for source in sources:
    object_file = \
      source.replace(".cc", "").replace(".cpp", "").replace(".c", "") + ".o"
    out, err = system(['g++', '-O3', '-funroll-loops', '-c', '-o', \
      object_file, source])
    out_message += out
    err_message += err
  out, err = system(['g++', '-O2', '-o', 'MyBot'] + safeglob('*.o') + ['-lm'])
  out_message += out
  err_message += err
  err_message += check_path('MyBot')
  return out_message, err_message

# This it the routine that is invoked to compile entries written in pure C
def compile_c():
  nukeglob('*.o')
  nukeglob('MyBot')
  sources = safeglob('*.c')
  for source in sources:
    object_file = source.replace(".c", "") + ".o"
    system(['gcc', '-O3', '-funroll-loops', '-c', '-o', object_file, source])

  system(['gcc', '-O2', '-o', 'MyBot'] + safeglob('*.o') + ['-lm'])
  check_path('MyBot')

def compile_go():
  nukeglob('*.6')
  nukeglob('MyBot')

  sources = safeglob('*.go')
  system(['/usr/local/bin/6g', '-o', '_go_.6'] + sources)
  system(['/usr/local/bin/6l', '-o', 'MyBot', '_go_.6'])
  check_path('MyBot')

# This it the routine that is invoked to compile entries written in Python
def compile_python():
  print "Python scripts need not be compiled"
  nukeglob('*.pyc')
  for script in safeglob('*.py'):
    os.chmod(script, 0644)
  check_path('MyBot.py')

# This it the routine that is invoked to compile entries written in Ruby
def compile_ruby():
  print "Ruby scripts need not be compiled"
  for script in safeglob('*.rb'):
    os.chmod(script, 0644)
  check_path('MyBot.rb')

# This it the routine that is invoked to compile entries written in Perl
def compile_perl():
  print "Perl scripts need not be compiled"
  for script in safeglob('*.pl'):
    os.chmod(script, 0644)
  check_path('MyBot.pl')

def compile_js():
  print "JavaScript scripts need not be compiled"
  for script in safeglob('*.js'):
    os.chmod(script, 0644)
  check_path('MyBot.js')

def compile_scheme():
  print "Scheme scripts need not be compiled"
  for script in safeglob('*.ss'):
    os.chmod(script, 0644)
  check_path('MyBot.ss')

def compile_lua():
  print "Lua scripts need not be compiled"
  for script in safeglob('*.lua'):
    os.chmod(script, 0644)
  check_path('MyBot.lua')

def compile_clojure():
  print "Clojure scripts need not be compiled"
  for script in safeglob('*.clj'):
    os.chmod(script, 0644)
  check_path('MyBot.clj')

def compile_ocaml():
  nukeglob('MyBot.native')
  system(['ocamlbuild', 'MyBot.native'])
  check_path('MyBot.native')

def compile_lisp():
  nukeglob('MyBot')
  system(['/usr/local/bin/make-clisp', 'MyBot.lisp'])
  check_path('MyBot.sbcl')

# This is the list of supported programming languages. The code for compiling
# each of these languages is located above.
programming_languages = [
  ("MyBot.java", "Java", compile_java),
  ("MyBot.cc", "C++", compile_cpp),
  ("MyBot.c", "C", compile_c),
  ("MyBot.py", "Python", compile_python),
  #("MyBot.rb", "Ruby", compile_ruby),
  #("MyBot.pl", "Perl", compile_perl)
  #("MyBot.hs", "Haskell", compile_haskell),
  #("MyBot.cs", "C#", compile_cs),
  #("MyBot.js", "JavaScript", compile_js),
  #("MyBot.go", "Go", compile_go),
  #("MyBot.ss", "Scheme", compile_scheme),
  #("MyBot.lua", "Lua", compile_lua),
  #("MyBot.clj", "Clojure", compile_clojure),
  #("MyBot.lisp", "Common Lisp", compile_lisp),
  #("MyBot.ml", "OCaml", compile_ocaml),
]

# Autodetects the language of the entry in the current working directory and
# compiles it.
def compile_anything():
  output = ""
  error = ""
  detected_langs = [
    lang for lang in programming_languages if os.path.exists(lang[0])
  ]
  # If no language was detected
  if len(detected_langs) == 0:
    error += "The auto-compile environment could not locate your main code\n"
    error += "file. This is probably because you accidentally changed the\n"
    error += "name of your main code file. You must include exactly one file\n"
    error += "with one of the following names:\n"
    for lang in programming_languages:
      error += "   * " + lang[0] + " (" + lang[1] + ")\n"
    error += "This is to help the auto-compile environment figure out which\n"
    error += "programming language you are using.\n"
    return output, error
  # If more than one language was detected
  if len(detected_langs) > 1:
    error = "The auto-compile environment found more than one main code "
    error += "file:\n"
    for lang in detected_langs:
      error += "   * " + lang[0] + " (" + lang[1] + ")\n"
    error += "You must submit only one of these files so that the "
    error += "auto-compile environment can figure out which programming "
    error += "language you are trying to use.\n"
    return output, error
  # If we get this far, then we have successfully auto-detected the language
  # that this contestant is using.
  main_code_file = detected_langs[0][0]
  detected_lang = detected_langs[0][1]
  compile_function = detected_langs[0][2]
  output += "Found " + main_code_file + ". Compiling this entry as " + \
    detected_lang + "\n"
  out, err = compile_function()
  output += out
  error += err
  return output, error

#out, err = compile_anything()
#print "OUT: " + out
#print "ERR: " + err
