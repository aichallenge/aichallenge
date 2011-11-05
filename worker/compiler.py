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
# To add a new language you must add an entry to the "languages" list.
#
# For example the entry for Python is as follows:
#    Language("Python", BOT +".py", "MyBot.py",
#        "python MyBot.py",
#        ["*.pyc"],
#        [(["*.py"], ChmodCompiler("Python"))]
#    ),
# This defines the output file as MyBot.py, removes all .pyc files, and runs
# all the found .py files through the ChmodCompiler, which is a pseudo-compiler
# class which only chmods the found files.
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

from sandbox import get_sandbox
from string import split

try:
    from server_info import server_info
    MEMORY_LIMIT = server_info.get('memory_limit', 1500)
except ImportError:
    MEMORY_LIMIT = 1500

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
    out = []
    errors = []
    sandbox.start(cmd)
    # flush stdout to keep stuff moving
    try:
        while (sandbox.is_alive and time.time() < timelimit):
            out_ln = sandbox.read_line((timelimit - time.time()) + 1)
            if out_ln:
                out.append(out_ln)
    finally:
        sandbox.kill()
    # capture final output for error reporting
    tmp = sandbox.read_line(1)
    while tmp:
        out.append(tmp)
        tmp = sandbox.read_line(1)

    if time.time() > timelimit:
        errors.append("Compilation timed out with command %s"
                % (cmd,))
    err_line = sandbox.read_error()
    while err_line is not None:
        errors.append(err_line)
        err_line = sandbox.read_error()
    return out, errors

def check_path(path, errors):
    if not os.path.exists(path):
        errors.append("Output file " + str(os.path.basename(path)) + " was not created.")
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
    def __init__(self, args, separate=False, out_files=[], out_ext=None):
        """Compile files using an external compiler.

        args, is a list of the compiler command and any arguments to run.
        separate, controls whether all input files are sent to the compiler
            in one command or one file per compiler invocation.
        out_files, is a list of files that should exist after each invocation
        out_ext, is an extension that is replaced on each input file and should
            exist after each invocation.
        """

        self.args = args
        self.separate = separate
        self.out_files = out_files
        self.out_ext = out_ext

    def __str__(self):
        return "ExternalCompiler: %s" % (' '.join(self.args),)

    def compile(self, bot_dir, globs, errors, timelimit):
        with CD(bot_dir):
            files = safeglob_multi(globs)

        box = get_sandbox(bot_dir)
        try:
            if self.separate:
                for filename in files:
                    cmdline = " ".join(self.args + [filename])
                    cmd_out, cmd_errors = _run_cmd(box, cmdline, timelimit)
                    cmd_errors = self.cmd_error_filter(cmd_out, cmd_errors);
                    if not cmd_errors:
                        for ofile in self.out_files:
                            box.check_path(ofile, cmd_errors)
                        if self.out_ext:
                            oname = os.path.splitext(filename)[0] + self.out_ext
                            box.check_path(oname, cmd_errors)
                        if cmd_errors:
                            cmd_errors += cmd_out
                    if cmd_errors:
                        errors += cmd_errors
                        return False
            else:
                cmdline = " ".join(self.args + files)
                cmd_out, cmd_errors = _run_cmd(box, cmdline, timelimit)
                cmd_errors = self.cmd_error_filter(cmd_out, cmd_errors);
                if not cmd_errors:
                    for ofile in self.out_files:
                        box.check_path(ofile, cmd_errors)
                    if self.out_ext:
                        for filename in files:
                            oname = os.path.splitext(filename)[0] + self.out_ext
                            box.check_path(oname, cmd_errors)
                    if cmd_errors:
                        cmd_errors += cmd_out
                if cmd_errors:
                    errors += cmd_errors
                    return False
            box.retrieve()
        finally:
            box.release()
        return True

    def cmd_error_filter(self, cmd_out, cmd_errors):
        """Default implementation doesn't filter"""
        return cmd_errors


# An external compiler with some stdout/sdtderr post-processing power
class ErrorFilterCompiler(ExternalCompiler):
    def __init__(self, args, separate=False, out_files=[], out_ext=None, stdout_is_error=False, skip_stdout=0, filter_stdout=None, filter_stderr=None):
        """Compile files using an external compiler.

        args, is a list of the compiler command and any arguments to run.
        separate, controls whether all input files are sent to the compiler
            in one command or one file per compiler invocation.
        out_files, is a list of files that should exist after each invocation
        out_ext, is an extension that is replaced on each input file and should
            exist after each invocation.
        stdout_is_error, controls if stdout contains error compiler error messages
        skip_stdout, controls how many lines at the start of stdout are ignored
        filter_stdout, is a regex that filters out lines from stdout that are no errors
        filter_stderr, is a regex that filters out lines from stderr that are no errors
        """

        ExternalCompiler.__init__(self, args, separate, out_files, out_ext)
        self.stdout_is_error = stdout_is_error
        self.skip_stdout = skip_stdout;
        if filter_stdout is None:
            self.stdout_re = None
        else:
            self.stdout_re = re.compile(filter_stdout)
        if filter_stderr is None:
            self.stderr_re = None
        else:
            self.stderr_re = re.compile(filter_stderr)

    def __str__(self):
        return "ErrorFilterCompiler: %s" % (' '.join(self.args),)

    def cmd_error_filter(self, cmd_out, cmd_errors):
        """Skip and filter lines"""
        if self.skip_stdout > 0:
            del cmd_out[:self.skip_stdout]
        # Somehow there are None values in the output
        if self.stdout_re is not None:
            cmd_out = [line for line in cmd_out if
                       line is None or not self.stdout_re.search(line)]
        if self.stderr_re is not None:
            cmd_errors = [line for line in cmd_errors if
                          line is None or not self.stderr_re.search(line)]
        if self.stdout_is_error:
            return [line for line in cmd_out if line is not None] + cmd_errors
        return cmd_errors

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

        box = get_sandbox(bot_dir)
        try:
            for source in sources:
                head, ext = os.path.splitext(source)
                if ext in self.replacements:
                    target = head + self.replacements[ext]
                else:
                    errors.append("Could not determine target for source file %s." % source)
                    return False
                cmdline = " ".join(self.args + [self.outflag, target, source])
                cmd_out, cmd_errors = _run_cmd(box, cmdline, timelimit)
                if cmd_errors:
                    errors += cmd_errors
                    return False
            box.retrieve()
        finally:
            box.release()
        return True

PYTHON_EXT_COMPILER = '''"from distutils.core import setup
from distutils.extension import read_setup_file
setup(ext_modules = read_setup_file('setup_exts'), script_args = ['-q', 'build_ext', '-i'])"'''

comp_args = {
    # lang : ([list of compilation arguments], ...)
    #                If the compilation should output each source file to
    #                its own object file, don't include the -o flags here,
    #                and use the TargetCompiler in the languages dict.
    "Ada"           : [["gcc-4.4", "-O3", "-funroll-loops", "-c"],
                             ["gnatbind"],
                             ["gnatlink", "-o", BOT]],
    "C"             : [["gcc", "-O3", "-funroll-loops", "-c"],
                             ["gcc", "-O2", "-lm", "-o", BOT]],
    "C#"            : [["gmcs", "-warn:0", "-out:%s.exe" % BOT]],
    "VB"            : [["vbnc", "-out:%s.exe" % BOT]],
    "C++"         : [["g++", "-O3", "-funroll-loops", "-c"],
                             ["g++", "-O2", "-lm", "-o", BOT]],
    "C++11"         : [["g++", "-O3", "-std=c++0x", "-c"],
                             ["g++", "-O2", "-lm", "-std=c++0x", "-o", BOT]],
    "D"             : [["dmd", "-O", "-inline", "-release", "-noboundscheck", "-of" + BOT]],
    "Go"            : [["6g", "-o", "_go_.6"],
                             ["6l", "-o", BOT, "_go_.6"]],
    "Groovy"    : [["groovyc"],
                             ["jar", "cfe", BOT + ".jar", BOT]],
    # If we ever upgrade to GHC 7, we will need to add -rtsopts to this command
    # in order for the maximum heap size RTS flag to work on the executable.
    "Haskell" : [["ghc", "--make", BOT + ".hs", "-O", "-v0"]],
    "Java"        : [["javac", "-J-Xmx%sm" % (MEMORY_LIMIT)],
                             ["jar", "cfe", BOT + ".jar", BOT]],
    "Lisp"      : [['sbcl', '--dynamic-space-size', str(MEMORY_LIMIT), '--script', BOT + '.lisp']],
    "OCaml"     : [["ocamlbuild -lib unix", BOT + ".native"]],
    "Pascal"    : [["fpc", "-Mdelphi", "-Si", "-O3", "-Xs", "-v0", "-o" + BOT]],
    "Python"    : [["python", "-c", PYTHON_EXT_COMPILER]],
    "Python3"   : [["python3", "-c", PYTHON_EXT_COMPILER]],
    "Scala"     : [["scalac"]],
    }

targets = {
    # lang : { old_ext : new_ext, ... }
    "C"     : { ".c" : ".o" },
    "C++" : { ".c" : ".o", ".cpp" : ".o", ".cc" : ".o" },
    }

Language = collections.namedtuple("Language",
        ['name', 'out_file', 'main_code_file', 'command', 'nukeglobs',
            'compilers']
        )

languages = (
    # Language(name, output file,
    #      main_code_file
    #      command_line
    #      [nukeglobs],
    #      [(source glob, compiler), ...])
    #
    # The compilers are run in the order given.
    # If a source glob is "" it means the source is part of the compiler
    #   arguments.
    Language("Ada", BOT, "mybot.adb",
        "./MyBot",
        ["*.ali"],
        [(["*.adb"], ExternalCompiler(comp_args["Ada"][0])),
            (["mybot.ali"], ExternalCompiler(comp_args["Ada"][1])),
            (["mybot.ali"], ExternalCompiler(comp_args["Ada"][2]))]
    ),
    Language("C", BOT, "MyBot.c",
        "./MyBot",
        ["*.o", BOT],
        [(["*.c"], TargetCompiler(comp_args["C"][0], targets["C"])),
            (["*.o"], ExternalCompiler(comp_args["C"][1]))]
    ),
    Language("C#", BOT +".exe", "MyBot.cs",
        "mono MyBot.exe",
        [BOT + ".exe"],
        [(["*.cs"], ExternalCompiler(comp_args["C#"][0]))]
    ),
    Language("VB", BOT +".exe", "MyBot.vb",
        "mono MyBot.exe",
        [BOT + ".exe"],
        [(["*.vb"],
            ExternalCompiler(comp_args["VB"][0], out_files=['MyBot.exe']))]
    ),
    # These two C++ variants should be combined after the ants contest
    Language("C++", BOT, "MyBot.cc",
        "./MyBot",
        ["*.o", BOT],
        [
            (["*.c", "*.cpp", "*.cc"],
                TargetCompiler(comp_args["C++"][0], targets["C++"])),
            (["*.o"], ExternalCompiler(comp_args["C++"][1]))
        ]
    ),
    Language("C++11", BOT, "MyBot.cpp",
        "./MyBot",
        ["*.o", BOT],
        [
            (["*.c", "*.cpp", "*.cc"],
                TargetCompiler(comp_args["C++11"][0], targets["C++"])),
            (["*.o"], ExternalCompiler(comp_args["C++11"][1]))
        ]
    ),
    Language("Clojure", BOT +".clj", "MyBot.clj",
		"java -Xmx%sm -cp /usr/share/java/clojure.jar:. clojure.main MyBot.clj" % (MEMORY_LIMIT,),
        [],
        [(["*.clj"], ChmodCompiler("Clojure"))]
    ),
    Language("CoffeeScript", BOT +".coffee", "MyBot.coffee",
        "coffee MyBot.coffee",
        [],
        [(["*.coffee"], ChmodCompiler("CoffeeScript"))]
    ),
    Language("D", BOT, "MyBot.d",
        "./MyBot",
        ["*.o", BOT],
        [(["*.d"], ExternalCompiler(comp_args["D"][0]))]
    ),
    Language("Erlang", "my_bot.beam", "my_bot.erl",
        "erl -hms"+ str(MEMORY_LIMIT) +"m -smp disable -noshell -s my_bot start -s init stop",
        ["*.beam"],
        [(["*.erl"], ExternalCompiler(["erlc"], out_ext=".beam"))]
    ),
    Language("Go", BOT, "MyBot.go",
        "./MyBot",
        ["*.8", "*.6", BOT],
        [(["*.go"], ExternalCompiler(comp_args["Go"][0], out_files=['_go_.6'])),
            ([""], ExternalCompiler(comp_args["Go"][1], out_files=['_go_.6']))]
    ),
    Language("Groovy", BOT +".jar", "MyBot.groovy",
        "java -Xmx" + str(MEMORY_LIMIT) + "m -cp MyBot.jar:/usr/share/groovy/embeddable/groovy-all-1.7.5.jar MyBot",
        ["*.class, *.jar"],
        [(["*.groovy"], ExternalCompiler(comp_args["Groovy"][0])),
        (["*.class"], ExternalCompiler(comp_args["Groovy"][1]))]
    ),
    Language("Haskell", BOT, "MyBot.hs",
        "./MyBot +RTS -M" + str(MEMORY_LIMIT) + "m",
        [BOT],
        [([""], ExternalCompiler(comp_args["Haskell"][0]))]
    ),
    Language("Java", BOT +".jar", "MyBot.java",
        "java -Xmx" + str(MEMORY_LIMIT) + "m -jar MyBot.jar",
        ["*.class", "*.jar"],
        [(["*.java"], ExternalCompiler(comp_args["Java"][0])),
            (["*.class"], ExternalCompiler(comp_args["Java"][1]))]
    ),
    Language("Javascript", BOT +".js", "MyBot.js",
        "node MyBot.js",
        [],
        [(["*.js"], ChmodCompiler("Javascript"))]
    ),
    Language("Lisp", BOT, "MyBot.lisp",
        "./MyBot --dynamic-space-size " + str(MEMORY_LIMIT),
        [BOT],
        [([""], ExternalCompiler(comp_args["Lisp"][0]))]
    ),
    Language("Lua", BOT +".lua", "MyBot.lua",
        "luajit-2.0.0-beta5 MyBot.lua",
        [],
        [(["*.lua"], ChmodCompiler("Lua"))]
    ),
    Language("OCaml", BOT +".native", "MyBot.ml",
        "./MyBot.native",
        [BOT + ".native"],
        [([""], ExternalCompiler(comp_args["OCaml"][0]))]
    ),
    Language("Pascal", BOT, BOT + ".pas",
        "./" + BOT,
        [BOT, "*.o", "*.ppu"],
        [([BOT + ".pas"], ErrorFilterCompiler(comp_args["Pascal"][0], 
           stdout_is_error=True, skip_stdout=2,
           filter_stderr='^/usr/bin/ld: warning: link.res contains output sections; did you forget -T\?$'))]
    ),
    Language("Perl", BOT +".pl", "MyBot.pl",
        "perl MyBot.pl",
        [],
        [(["*.pl"], ChmodCompiler("Perl"))]
    ),
    Language("PHP", BOT +".php", "MyBot.php",
        "php MyBot.php",
        [],
        [(["*.php"], ChmodCompiler("PHP"))]
    ),
    Language("Python", BOT +".py", "MyBot.py",
        "python MyBot.py",
        ["*.pyc"],
        [(["*.py"], ChmodCompiler("Python")),
        (["setup_exts"], ErrorFilterCompiler(comp_args["Python"][0], separate=True, filter_stderr='-Wstrict-prototypes'))]
    ),
    Language("Python3", BOT +".py3", "MyBot.py3",
        "python3 MyBot.py3",
        ["*.pyc"],
        [(["*.py3"], ChmodCompiler("Python3")),
        (["setup_exts"], ErrorFilterCompiler(comp_args["Python3"][0], separate=True, filter_stderr='-Wstrict-prototypes'))]
    ),
    Language("Ruby", BOT +".rb", "MyBot.rb",
        "ruby MyBot.rb",
        [],
        [(["*.rb"], ChmodCompiler("Ruby"))]
    ),
    Language("Scala", BOT +".scala", "MyBot.scala",
        'scala -J-Xmx'+ str(MEMORY_LIMIT) +'m -howtorun:object MyBot',
        ["*.scala, *.jar"],
        [(["*.scala"], ExternalCompiler(comp_args["Scala"][0]))]
    ),
    Language("Scheme", BOT +".ss", "MyBot.ss",
        "./MyBot",
        [],
        [(["*.ss"], ChmodCompiler("Scheme"))]
    ),
    Language("Tcl", BOT +".tcl", "MyBot.tcl",
        "tclsh8.5 MyBot.tcl",
        [],
        [(["*.tcl"], ChmodCompiler("Tcl"))]
    ),
)


def compile_function(language, bot_dir, timelimit):
    """Compile submission in the current directory with a specified language."""
    with CD(bot_dir):
        for glob in language.nukeglobs:
            nukeglob(glob)

    errors = []
    stop_time = time.time() + timelimit
    for globs, compiler in language.compilers:
        try:
            if not compiler.compile(bot_dir, globs, errors, stop_time):
                return False, errors
        except StandardError, exc:
            raise
            errors.append("Compiler %s failed with: %s"
                    % (compiler, exc))
            return False, errors

    compiled_bot_file = os.path.join(bot_dir, language.out_file)
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

def compile_anything(bot_dir, timelimit=600, max_error_len = 3072):
    """Autodetect the language of an entry and compile it."""
    detected_language, errors = detect_language(bot_dir)
    if detected_language:
        # If we get this far, then we have successfully auto-detected
        # the language that this entry is using.
        compiled, errors = compile_function(detected_language, bot_dir,
                timelimit)
        if compiled:
            name = detected_language.name
            run_cmd = detected_language.command
            run_filename = os.path.join(bot_dir, '../run.sh')
            with open(run_filename, 'w') as f:
                f.write('#%s\n%s\n' % (name, run_cmd))
            return name, None
        else:
            # limit length of reported errors
            if len(errors) > 0 and sum(map(len, errors)) > max_error_len:
                first_errors = []
                cur_error = 0
                length = len(errors[0])
                while length < (max_error_len / 3): # take 1/3 from start
                    first_errors.append(errors[cur_error])
                    cur_error += 1
                    length += len(errors[cur_error])
                first_errors.append("...")
                length += 3
                end_errors = []
                cur_error = -1
                while length <= max_error_len:
                    end_errors.append(errors[cur_error])
                    cur_error -= 1
                    length += len(errors[cur_error])
                end_errors.reverse()
                errors = first_errors + end_errors

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
