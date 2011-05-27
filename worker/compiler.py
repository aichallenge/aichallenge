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

import sys
import os
import re
import glob
import subprocess
import fnmatch
import errno
import shutil
import json
from optparse import OptionParser

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

def system(args, errors):
    proc = subprocess.Popen(args, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    (out, err) = proc.communicate()
    if err:
        errors.append(err)
    if proc.returncode != 0:
        errors.append("Command '%s' had error return code %d"
                % (" ".join(args), proc.returncode))
    return proc.returncode == 0

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

    def compile(self, globs, errors):
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

    def __repr__(self):
        return 'ExternalCompiler: ' + ' '.join(self.args)

    def compile(self, globs, errors):
        files = safeglob_multi(globs)
        if self.separate:
            for file in files:
                if not system(self.args + [file], errors):
                    return False
        else:
            if not system(self.args + files, errors):
                return False
        return True

# Compiles each file to its own output, based on the replacements dict.
class TargetCompiler(Compiler):
    def __init__(self, args, replacements, outflag="-o"):
        self.args = args
        self.replacements = replacements
        self.outflag = outflag

    def compile(self, globs, errors):
        sources = safeglob_multi(globs)
        for source in sources:
            head, ext = os.path.splitext(source)
            if ext in self.replacements:
                target = head + self.replacements[ext]
            else:
                errors.append("Could not determine target for source file %s." % source)
                return False
            if not system(self.args + [self.outflag, target, source], errors):
                return False
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
    "Go"            : [["/usr/local/bin/6g", "-o", "_go_.6"],
                             ["/usr/local/bin/6l", "-o", BOT, "_go_.6"]],
    "Groovy"    : [["groovyc"],
                             ["jar", "cfe", BOT + ".jar", BOT]],
    "Haskell" : [["ghc", "--make", BOT + ".hs", "-O", "-v0"]],
    "Java"        : [["javac"],
                             ["jar", "cfe", BOT + ".jar", BOT]],
    "Lisp"      : [['sbcl', '--dynamic-space-size', '256', '--script', BOT + '.lisp']],
    "OCaml"     : [["ocamlbuild", BOT + ".native"]],
    "Scala"     : [["scalac"]],
    }

targets = {
    # lang : { old_ext : new_ext, ... }
    "C"     : { ".c" : ".o" },
    "C++" : { ".c" : ".o", ".cpp" : ".o", ".cc" : ".o" },
    }

# TODO: turn these into objects or dicts
languages = {
    # lang :
    #     (output extension,
    #      command_line
    #      [nukeglobs],
    #      [(source glob, compiler), ...])
    #
    # The compilers are run in the order given.
    # If the extension is "" it means the output file is just BOT
    # If a source glob is "" it means the source is part of the compiler
    #   arguments.
    "C":
        ("",
         "MyBot.c",
         "./MyBot",
         ["*.o", BOT],
         [(["*.c"], TargetCompiler(comp_args["C"][0], targets["C"])),
          (["*.o"], ExternalCompiler(comp_args["C"][1]))]
        ),
    "C#":
        (".exe",
         "MyBot.cs",
         "mono MyBot.exe",
         [BOT + ".exe"],
         [(["*.cs"], ExternalCompiler(comp_args["C#"][0]))]
        ),
    "C++":
        ("",
         "MyBot.cc",
         "./MyBot",
         ["*.o", BOT],
         [(["*.c", "*.cpp", "*.cc"], TargetCompiler(comp_args["C++"][0], targets["C++"])),
          (["*.o"], ExternalCompiler(comp_args["C++"][1]))]
         ),
    "Clojure":
        (".clj",
         "?",
         "?",
         [],
         [(["*.clj"], ChmodCompiler("Clojure"))]
        ),
    "CoffeeScript":
        (".coffee",
         "MyBot.coffee",
         "coffee MyBot.coffee",
         [],
         [(["*.coffee"], ChmodCompiler("CoffeeScript"))]
        ),
    "D":
        ("",
         "MyBot.d",
         "./MyBot",
         ["*.o", BOT],
         [(["*.d"], ExternalCompiler(comp_args["D"][0]))]
        ),
    "Go":
        ("",
         "MyBot.go",
         "./MyBot",
         ["*.8", BOT],
         [(["*.go"], ExternalCompiler(comp_args["Go"][0])),
          ([""], ExternalCompiler(comp_args["Go"][1]))]
        ),
    "Groovy":
        (".jar",
         "MyBot.groovy",
         "java -cp MyBot.jar:/usr/share/groovy/embeddable/groovy-all-1.7.5.jar MyBot",
         ["*.class, *.jar"],
         [(["*.groovy"], ExternalCompiler(comp_args["Groovy"][0])),
         (["*.class"], ExternalCompiler(comp_args["Groovy"][1]))]
        ),
    "Haskell":
        ("",
         "MyBot.hs",
         "./MyBot",
         [BOT],
         [([""], ExternalCompiler(comp_args["Haskell"][0]))]
        ),
    "Java":
        (".jar",
         "MyBot.java",
         "java -jar MyBot.jar",
         ["*.class", "*.jar"],
         [(["*.java"], ExternalCompiler(comp_args["Java"][0])),
         (["*.class"], ExternalCompiler(comp_args["Java"][1]))]
        ),
    "Javascript":
        (".js",
         "MyBot.js",
         "node MyBot.js",
         [],
         [(["*.js"], ChmodCompiler("Javascript"))]
        ),
    "Lisp":
        ("",
         "MyBot.lisp",
         "./MyBot --dynamic-space-size 256",
        [BOT],
        [([""], ExternalCompiler(comp_args["Lisp"][0]))]),
    "Lua":
        (".lua",
         "MyBot.lua",
         "?",
         [],
         [(["*.lua"], ChmodCompiler("Lua"))]
        ),
    "OCaml":
        (".native",
         "MyBot.ml",
         "./MyBot.native",
         [BOT + ".native"],
         [([""], ExternalCompiler(comp_args["OCaml"][0]))]
        ),
    "Perl":
        (".pl",
         "MyBot.pl",
         "perl MyBot.pl",
         [],
         [(["*.pl"], ChmodCompiler("Perl"))]
        ),
    "PHP":
        (".php",
         "MyBot.php",
         "php MyBot.php",
         [],
         [(["*.php"], ChmodCompiler("PHP"))]
        ),
    "Python":
        (".py",
         "MyBot.py",
         "python MyBot.py",
        ["*.pyc"],
        [(["*.py"], ChmodCompiler("Python"))]),
    "Ruby":
        (".rb",
         "MyBot.rb",
         "ruby MyBot.rb",
         [],
         [(["*.rb"], ChmodCompiler("Ruby"))]
        ),
    "Scala":
        (".scala",
         "MyBot.scala",
         "?",
         ["*.scala, *.jar"],
         [(["*.scala"], ExternalCompiler(comp_args["Scala"][0]))]
        ),
    "Scheme":
        (".ss",
         "MyBot.ss",
         "./MyBot",
         [],
         [(["*.ss"], ChmodCompiler("Scheme"))]
        ),
    }


def compile_function(language, errors):
    """Compile submission in the current directory with a specified language."""
    extension, main_code_file, command, nukeglobs, compilers = languages[language]

    for glob in nukeglobs:
        nukeglob(glob)

    for globs, compiler in compilers:
        try:
            if not compiler.compile(globs, errors):
                return False
        except:
            errors.append("Compiler failed: " + str(compiler))
            return False

    return check_path(BOT + extension, errors)

def detect_language(bot_dir):
    """Try and detect what language a submission is using"""
    with CD(bot_dir):
        # Autodetects the language of the entry in the current working directory
        detected_lang = get_run_lang(bot_dir)
        if detected_lang and detected_lang in languages:
            detected_langs = [languages[detected_lang] + (detected_lang,)]
        else:
            detected_langs = [
                lang_data + (lang_name,) for lang_name, lang_data
                in languages.items() if os.path.exists(lang_data[1])
            ]

        # If no language was detected
        if len(detected_langs) > 1:
            return None, ['Found multiple MyBot.* files: \n'+
                          '\n'.join([lang[1] for lang in detected_langs])]
        elif len(detected_langs) == 0:
            return None, ['Did not find MyBot.* file.\nPlease add one of the following filename to your zip file:\n'
                          +'\n'.join(key + ": " + val[1] for key, val in language.items())]
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
    with CD(bot_dir):
        detected_language, errors = detect_language(bot_dir)
        if detected_language:
            # If we get this far, then we have successfully auto-detected
            # the language that this entry is using.
            main_code_file = detected_language[1]
            detected_lang = detected_language[-1]
            run_cmd = detected_language[2]
            errors = []
            if compile_function(detected_lang, errors):
                with open('../run.sh', 'w') as f:
                    f.write('#%s\n%s\n' % (detected_lang, run_cmd))
                return detected_lang, None
            else:
                return detected_lang, errors
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
