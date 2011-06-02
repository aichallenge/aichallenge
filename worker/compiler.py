#!/usr/bin/python
# compiler.py
# Author: Jeff Cameron (jeff@jpcameron.com)
#
# Auto-detects the language of the entry based on the extension,
# attempts to compile it, returning the stdout and stderr.
# The auto-detection works by looking for the "main" code file of
# the available languages. If the number of matching languages is 0 or
# more than 1, it is an error, and an appropriate error message is returned.
#
# To add a new language you must add an entry to the "languages" dictionary in
# the following format:
#     LanguageName : (extension, [NukeGlobs], [(Compile_globs, Compile_class)])
#
# For example the entry for Python is as follows:
#     "Python" : (".py", ["*.pyc"], [("*.py", ChmodCompiler("Python"))]).
# This defines the extension as .py, removes all .pyc files, and runs all the
# found .py files through the ChmodCompiler, which is a pseudo-compiler class
# which only chmods the found files.
#
# If you want to run a real compiler then you need to define a set of flags to
# send it. In this case you would either use TargetCompiler or ExternalCompiler.
# The difference between the two is the TargetCompiler iterates over the found
# files and creates object files from them, whereas the External doesn't.
# If in doubt just stick to the ExternalCompiler.
#
# An example is from the C# Entry:
#     "C#" : (".exe", ["*.exe"],
#                     [(["*.cs"], ExternalCompiler(comp_args["C#"][0]))])
#
# To make the dictionary more readable the flags have been split into a
# separate "comp_args" dictionary. C#'s entry looks like so:
#     "C#" : [["gmcs", "-warn:0", "-out:%s.exe" % BOT]]
# At runtime this all boils down to:
#     gmcs -warn:0 -out:MyBot.exe *.cs
# (though the *.cs is actually replaced with the list of files found)

import collections
import errno
import fnmatch
import glob
import json
import os
import re
import shutil
import subprocess
import sys
import time
from optparse import OptionParser

from sandbox import Sandbox

try:
    from server_info import server_info
    MEMORY_LIMIT = server_info.get('memory_limit', 500)
    COMPILE_TIME = server_info.get('compile_time', 10 * 60)
except ImportError:
    MEMORY_LIMIT = 500
    COMPILE_TIME = 10 * 60

BOT = "MyBot"
SAFEPATH = re.compile('[a-zA-Z0-9_.$-]+$')

class CD(object):
    def __init__(self, new_dir):
        self.new_dir = new_dir

    def __enter__(self):
        self.org_dir = os.getcwd()
        os.chdir(self.new_dir)
        return self.new_dir

    def __exit__(self, type, value, traceback):
        os.chdir(self.org_dir)

def safeglob(pattern):
    safepaths = []
    for root, dirs, files in os.walk("."):
        files = fnmatch.filter(files, pattern)
        for fname in files:
            if SAFEPATH.match(fname):
                safepaths.append(os.path.join(root, fname))
    return safepaths

def safeglob_multi(patterns):
    safepaths = []
    for pattern in patterns:
        safepaths.extend(safeglob(pattern))
    return safepaths

def nukeglob(pattern):
    paths = safeglob(pattern)
    for path in paths:
        # Ought to be all files, not folders
        try:
            os.unlink(path)
        except OSError, e:
            if e.errno != errno.ENOENT:
                raise

def _run_cmd(sandbox, cmd, timelimit):
    errors = []
    sandbox.start(cmd)
    try:
        while (sandbox.is_alive and time.time() < timelimit):
            sandbox.read_line((timelimit - time.time()) + 1)
    finally:
        sandbox.kill()
    if time.time() > timelimit:
        errors.append("Compilation timed out with file %s"
                % (filename,))
    err_line = sandbox.read_error()
    while err_line is not None:
        errors.append(err_line)
        err_line = sandbox.read_error()
    return errors

def check_path(path, errors):
    if not os.path.exists(path):
        errors.append("Output file " + str(path) + " was not created.")
        return False
    else:
        return True

class Compiler:
    def compile(self, globs, errors):
        raise NotImplementedError

class ChmodCompiler(Compiler):
    def __init__(self, language):
        self.language = language

    def __str__(self):
        return "ChmodCompiler: %s" % (self.language,)

    def compile(self, bot_dir, globs, errors, timelimit):
        with CD(bot_dir):
            for f in safeglob_multi(globs):
                try:
                    os.chmod(f, 0644)
                except Exception, e:
                    errors.append("Error chmoding %s - %s\n" % (f, e))
        return True

class ExternalCompiler(Compiler):
    def __init__(self, args, separate=False):
        self.args = args
        self.separate = separate

    def __str__(self):
        return "ExternalCompiler: %s" % (' '.join(self.args),)

    def compile(self, bot_dir, globs, errors, timelimit):
        with CD(bot_dir):
            files = safeglob_multi(globs)

        errored = False
        box = Sandbox(bot_dir)
        try:
            if self.separate:
                for filename in files:
                    cmdline = " ".join(self.args + [filename])
                    cmd_errors = _run_cmd(box, cmdline, timelimit)
                    if cmd_errors:
                        errors += cmd_errors
                        return False
            else:
                cmdline = " ".join(self.args + files)
                cmd_errors = _run_cmd(box, cmdline, timelimit)
                if cmd_errors:
                    errors += cmd_errors
                    return False
            box.retrieve()
        finally:
            box.release()
        return True

# Compiles each file to its own output, based on the replacements dict.
class TargetCompiler(Compiler):
    def __init__(self, args, replacements, outflag="-o"):
        self.args = args
        self.replacements = replacements
        self.outflag = outflag

    def __str__(self):
        return "TargetCompiler: %s" % (' '.join(self.args),)

    def compile(self, bot_dir, globs, errors, timelimit):
        with CD(bot_dir):
            sources = safeglob_multi(globs)

        box = Sandbox(bot_dir)
        try:
            for source in sources:
                head, ext = os.path.splitext(source)
                if ext in self.replacements:
                    target = head + self.replacements[ext]
                else:
                    errors.append("Could not determine target for source file %s." % source)
                    return False
                cmdline = " ".join(self.args + [self.outflag, target, source])
                cmd_errors = _run_cmd(box, cmdline, timelimit)
                if cmd_errors:
                    errors += cmd_errors
                    return False
            box.retrieve()
        finally:
            box.release()
        return True

comp_args = {
    # lang : ([list of compilation arguments], ...)
    #                If the compilation should output each source file to
    #                its own object file, don't include the -o flags here,
    #                and use the TargetCompiler in the languages dict.
    "C"             : [["gcc", "-O3", "-funroll-loops", "-c"],
                             ["gcc", "-O2", "-lm", "-o", BOT]],
    "C#"            : [["gmcs", "-warn:0", "-out:%s.exe" % BOT]],
    "C++"         : [["g++", "-O3", "-funroll-loops", "-c"],
                             ["g++", "-O2", "-lm", "-o", BOT]],
    "D"             : [["dmd", "-O", "-inline", "-release", "-of" + BOT]],
    "Go"            : [["6g", "-o", "_go_.6"],
                             ["6l", "-o", BOT, "_go_.6"]],
    "Groovy"    : [["groovyc"],
                             ["jar", "cfe", BOT + ".jar", BOT]],
    "Haskell" : [["ghc", "--make", BOT + ".hs", "-O", "-v0"]],
    "Java"        : [["javac"],
                             ["jar", "cfe", BOT + ".jar", BOT]],
    "Lisp"      : [['sbcl', '--dynamic-space-size', str(MEMORY_LIMIT), '--script', BOT + '.lisp']],
    "OCaml"     : [["ocamlbuild", BOT + ".native"]],
    "Scala"     : [["scalac"]],
    }

targets = {
    # lang : { old_ext : new_ext, ... }
    "C"     : { ".c" : ".o" },
    "C++" : { ".c" : ".o", ".cpp" : ".o", ".cc" : ".o" },
    }

Language = collections.namedtuple("Language",
        ['name', 'extension', 'main_code_file', 'command', 'nukeglobs',
            'compilers']
        )

languages = (
    # Language(name, output extension,
    #      main_code_file
    #      command_line
    #      [nukeglobs],
    #      [(source glob, compiler), ...])
    #
    # The compilers are run in the order given.
    # If the extension is "" it means the output file is just BOT
    # If a source glob is "" it means the source is part of the compiler
    #   arguments.
    Language("C", "", "MyBot.c",
        "./MyBot",
        ["*.o", BOT],
        [(["*.c"], TargetCompiler(comp_args["C"][0], targets["C"])),
            (["*.o"], ExternalCompiler(comp_args["C"][1]))]
    ),
    Language("C#", ".exe", "MyBot.cs",
        "mono MyBot.exe",
        [BOT + ".exe"],
        [(["*.cs"], ExternalCompiler(comp_args["C#"][0]))]
    ),
    Language("C++", "", "MyBot.cc",
        "./MyBot",
        ["*.o", BOT],
        [
            (["*.c", "*.cpp", "*.cc"],
                TargetCompiler(comp_args["C++"][0], targets["C++"])),
            (["*.o"], ExternalCompiler(comp_args["C++"][1]))
        ]
    ),
    Language("Clojure", ".clj", "MyBot.clj",
        "java -Xmx%sm -cp /usr/share/java/clojure.jar clojure.main MyBot.clj" % (MEMORY_LIMIT,),
        [],
        [(["*.clj"], ChmodCompiler("Clojure"))]
    ),
    Language("CoffeeScript", ".coffee", "MyBot.coffee",
        "coffee MyBot.coffee",
        [],
        [(["*.coffee"], ChmodCompiler("CoffeeScript"))]
    ),
    Language("D", "", "MyBot.d",
        "./MyBot",
        ["*.o", BOT],
        [(["*.d"], ExternalCompiler(comp_args["D"][0]))]
    ),
    Language("Go", "", "MyBot.go",
        "./MyBot",
        ["*.8", "*.6", BOT],
        [(["*.go"], ExternalCompiler(comp_args["Go"][0])),
            ([""], ExternalCompiler(comp_args["Go"][1]))]
    ),
    Language("Groovy", ".jar", "MyBot.groovy",
        "java -Xmx" + str(MEMORY_LIMIT) + "m -cp MyBot.jar:/usr/share/groovy/embeddable/groovy-all-1.7.5.jar MyBot",
        ["*.class, *.jar"],
        [(["*.groovy"], ExternalCompiler(comp_args["Groovy"][0])),
        (["*.class"], ExternalCompiler(comp_args["Groovy"][1]))]
    ),
    Language("Haskell", "", "MyBot.hs",
        "./MyBot",
        [BOT],
        [([""], ExternalCompiler(comp_args["Haskell"][0]))]
    ),
    Language("Java", ".jar", "MyBot.java",
        "java -Xmx" + str(MEMORY_LIMIT) + "m -jar MyBot.jar",
        ["*.class", "*.jar"],
        [(["*.java"], ExternalCompiler(comp_args["Java"][0])),
            (["*.class"], ExternalCompiler(comp_args["Java"][1]))]
    ),
    Language("Javascript", ".js", "MyBot.js",
        "node MyBot.js",
        [],
        [(["*.js"], ChmodCompiler("Javascript"))]
    ),
    Language("Lisp", "", "MyBot.lisp",
        "./MyBot --dynamic-space-size " + str(MEMORY_LIMIT),
        [BOT],
        [([""], ExternalCompiler(comp_args["Lisp"][0]))]
    ),
    Language("Lua", ".lua", "MyBot.lua",
        "?",
        [],
        [(["*.lua"], ChmodCompiler("Lua"))]
    ),
    Language("OCaml", ".native", "MyBot.ml",
        "./MyBot.native",
        [BOT + ".native"],
        [([""], ExternalCompiler(comp_args["OCaml"][0]))]
    ),
    Language("Perl", ".pl", "MyBot.pl",
        "perl MyBot.pl",
        [],
        [(["*.pl"], ChmodCompiler("Perl"))]
    ),
    Language("PHP", ".php", "MyBot.php",
        "php MyBot.php",
        [],
        [(["*.php"], ChmodCompiler("PHP"))]
    ),
    Language("Python", ".py", "MyBot.py",
        "python MyBot.py",
        ["*.pyc"],
        [(["*.py"], ChmodCompiler("Python"))]
    ),
    Language("Python3", ".py3", "MyBot.py3",
        "python3 MyBot.py3",
        ["*.pyc"],
        [(["*.py3"], ChmodCompiler("Python3"))]
    ),
    Language("Ruby", ".rb", "MyBot.rb",
        "ruby MyBot.rb",
        [],
        [(["*.rb"], ChmodCompiler("Ruby"))]
    ),
    Language("Scala", ".scala", "MyBot.scala",
        "?",
        ["*.scala, *.jar"],
        [(["*.scala"], ExternalCompiler(comp_args["Scala"][0]))]
    ),
    Language("Scheme", ".ss", "MyBot.ss",
        "./MyBot",
        [],
        [(["*.ss"], ChmodCompiler("Scheme"))]
    ),
)


def compile_function(language, bot_dir):
    """Compile submission in the current directory with a specified language."""
    with CD(bot_dir):
        for glob in language.nukeglobs:
            nukeglob(glob)

    errors = []
    timelimit = time.time() + COMPILE_TIME
    for globs, compiler in language.compilers:
        try:
            if not compiler.compile(bot_dir, globs, errors, timelimit):
                return False, errors
        except StandardError, exc:
            raise
            errors.append("Compiler %s failed with: %s"
                    % (compiler, exc))
            return False, errors

    compiled_bot_file = os.path.join(bot_dir, BOT + language.extension)
    return check_path(compiled_bot_file, errors), errors

_LANG_NOT_FOUND = """Did not find a recognized MyBot.* file.
Please add one of the following filenames to your zip file:
%s"""

def detect_language(bot_dir):
    """Try and detect what language a submission is using"""
    with CD(bot_dir):
        # Autodetects the language of the entry in the current working directory
        detected_langs = [
            lang for lang in languages if os.path.exists(lang.main_code_file)
        ]

        # If no language was detected
        if len(detected_langs) > 1:
            return None, ['Found multiple MyBot.* files: \n'+
                          '\n'.join([l.main_code_file for l in detected_langs])]
        elif len(detected_langs) == 0:
            return None, [_LANG_NOT_FOUND % (
                '\n'.join(l.name +": "+ l.main_code_file for l in languages),)]
        else:
            return detected_langs[0], None

def get_run_cmd(submission_dir):
    """Get the language of a submission"""
    with CD(submission_dir):
        if os.path.exists('run.sh'):
            with open('run.sh') as f:
                for line in f:
                    if line[0] != '#':
                        return line.rstrip('\r\n')

def get_run_lang(submission_dir):
    """Get the command to run a submission"""
    with CD(submission_dir):
        if os.path.exists('run.sh'):
            with open('run.sh') as f:
                for line in f:
                    if line[0] == '#':
                        return line[1:-1]

def compile_anything(bot_dir):
    """Autodetect the language of an entry and compile it."""
    detected_language, errors = detect_language(bot_dir)
    if detected_language:
        # If we get this far, then we have successfully auto-detected
        # the language that this entry is using.
        compiled, errors = compile_function(detected_language, bot_dir)
        if compiled:
            name = detected_language.name
            run_cmd = detected_language.command
            run_filename = os.path.join(bot_dir, '../run.sh')
            with open(run_filename, 'w') as f:
                f.write('#%s\n%s\n' % (name, run_cmd))
            return name, None
        else:
            return detected_language.name, errors
    else:
        return "Unknown", errors

def main(argv=sys.argv):
    parser = OptionParser(usage="Usage: %prog [options] [directory]")
    parser.add_option("-j", "--json", action="store_true", dest="json",
            default=False,
            help="Give compilation results in json format")
    options, args = parser.parse_args(argv)
    if len(args) == 1:
        detected_lang, errors = compile_anything(os.getcwd())
    elif len(args) == 2:
        detected_lang, errors = compile_anything(args[1])
    else:
        parser.error("Extra arguments found, use --help for usage")
    if options.json:
        import json
        print json.dumps([detected_lang, errors])
    else:
        print "Detected language:", detected_lang
        if errors != None and len(errors) != 0:
            for error in errors:
                print(error)

if __name__ == "__main__":
    main()
