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
import MySQLdb
from server_info import server_info

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

def compile_function(language):
  if language == "Java":
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
  if language == "Haskell":
    nukeglob('MyBot')
    out, err = system(['ghc', '--make', 'MyBot.hs', '-O2', '-v0'])
    err += check_path('MyBot')
    return out, err
  if language == "C#":
    nukeglob('MyBot.exe')
    sources = safeglob('*.cs')
    out, err = system(['gmcs', '-warn:0', '-out:MyBot.exe'] + sources)
    err += check_path('MyBot.exe')
    return out, err
  if language == "C++":
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
  if language == "C":
    nukeglob('*.o')
    nukeglob('MyBot')
    sources = safeglob('*.c')
    for source in sources:
      object_file = source.replace(".c", "") + ".o"
      system(['gcc', '-O3', '-funroll-loops', '-c', '-o', object_file, source])
    system(['gcc', '-O2', '-o', 'MyBot'] + safeglob('*.o') + ['-lm'])
    check_path('MyBot')
  if language == "Go":
    nukeglob('*.6')
    nukeglob('MyBot')
    sources = safeglob('*.go')
    system(['/usr/local/bin/6g', '-o', '_go_.6'] + sources)
    system(['/usr/local/bin/6l', '-o', 'MyBot', '_go_.6'])
    check_path('MyBot')
  if language == "Python":
    nukeglob('*.pyc')
    for script in safeglob('*.py'):
      os.chmod(script, 0644)
    check_path('MyBot.py')
    return "Python scripts do not need to be compiled.", ""
  if language == "Ruby":
    print "Ruby scripts need not be compiled"
    for script in safeglob('*.rb'):
      os.chmod(script, 0644)
    check_path('MyBot.rb')
  if language == "Perl":
    print "Perl scripts need not be compiled"
    for script in safeglob('*.pl'):
      os.chmod(script, 0644)
    check_path('MyBot.pl')
  if language == "Javascript":
    print "JavaScript scripts need not be compiled"
    for script in safeglob('*.js'):
      os.chmod(script, 0644)
    check_path('MyBot.js')
  if language == "Scheme":
    print "Scheme scripts need not be compiled"
    for script in safeglob('*.ss'):
      os.chmod(script, 0644)
    check_path('MyBot.ss')
  if language == "Lua":
    print "Lua scripts need not be compiled"
    for script in safeglob('*.lua'):
      os.chmod(script, 0644)
    check_path('MyBot.lua')
  if language == "Clojure":
    print "Clojure scripts need not be compiled"
    for script in safeglob('*.clj'):
      os.chmod(script, 0644)
    check_path('MyBot.clj')
  if language == "Ocaml":
    nukeglob('MyBot.native')
    system(['ocamlbuild', 'MyBot.native'])
    check_path('MyBot.native')
  if language == "Lisp":
    nukeglob('MyBot')
    system(['/usr/local/bin/make-clisp', 'MyBot.lisp'])
    check_path('MyBot.sbcl')

def get_programming_languages():
  connection = MySQLdb.connect(host = server_info["db_host"],
                               user = server_info["db_username"],
                               passwd = server_info["db_password"],
                               db = server_info["db_name"])
  cursor = connection.cursor(MySQLdb.cursors.DictCursor)
  cursor.execute("SELECT * FROM languages")
  programming_languages = cursor.fetchall()
  cursor.close()
  connection.close()
  return programming_languages

# Autodetects the language of the entry in the current working directory and
# compiles it.
def compile_anything():
  output = ""
  error = ""
  programming_languages = get_programming_languages()
  detected_langs = [
    lang for lang in programming_languages \
      if os.path.exists(lang["main_code_file"])
  ]
  # If no language was detected
  if len(detected_langs) == 0:
    error += "The auto-compile environment could not locate your main code\n"
    error += "file. This is probably because you accidentally changed the\n"
    error += "name of your main code file. You must include exactly one file\n"
    error += "with one of the following names:\n"
    for lang in programming_languages:
      error += "   * " + lang["main_code_file"] + " (" + lang["name"] + ")\n"
    error += "This is to help the auto-compile environment figure out which\n"
    error += "programming language you are using.\n"
    return output, error, "NULL"
  # If more than one language was detected
  if len(detected_langs) > 1:
    error = "The auto-compile environment found more than one main code "
    error += "file:\n"
    for lang in detected_langs:
      error += "   * " + lang["main_code_file"] + " (" + lang["name"] + ")\n"
    error += "You must submit only one of these files so that the "
    error += "auto-compile environment can figure out which programming "
    error += "language you are trying to use.\n"
    return output, error, "NULL"
  # If we get this far, then we have successfully auto-detected the language
  # that this contestant is using.
  main_code_file = detected_langs[0]["main_code_file"]
  detected_lang = detected_langs[0]["name"]
  language_id = detected_langs[0]["language_id"]
  output += "Found " + main_code_file + ". Compiling this entry as " + \
    detected_lang + "\n"
  out, err = compile_function(detected_lang)
  output += out
  error += err
  return output, error, language_id
