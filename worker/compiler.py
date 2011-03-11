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

import os
import re
import glob
import subprocess
import fnmatch
import errno
import time
import shutil
import MySQLdb
from server_info import server_info

BOT = "MyBot"
SAFEPATH = re.compile('[a-zA-Z0-9_.$-]+$')

def safeglob(pattern):
    safepaths = []
    paths = glob.glob(pattern)
    for path in paths:
        if SAFEPATH.match(path):
            safepaths.append(path)
    return safepaths

def safeglob_multi(patterns):
    safepaths = []
    for pattern in patterns:
        safepaths.extend(safeglob(pattern))
    return safepaths

def nukeglob(pattern):
    paths = glob.glob(pattern)
    for path in paths:
        try:
            if os.path.isdir(path):
                shutil.rmtree(path)
            else:
                os.unlink(path)
        except OSError, e:
            if e.errno != errno.ENOENT:
                raise

class Log:
    def __init__(self):
        self.out = ""
        self.err = ""

def system(args, log):
    log.out += ' '.join(args) + "\n"
    proc = subprocess.Popen(args, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    (out, err) = proc.communicate()
    log.out += out
    log.err += err
    return proc.returncode == 0

def check_path(path, log):
    if not os.path.exists(path):
        log.err += "\nFailure: output file " + str(path) + " was not created.\n"
        return False
    else:
        return True

class Compiler:
    def compile(self, globs, log):
        raise NotImplementedError

class ChmodCompiler(Compiler):
    def __init__(self, language):
        self.language = language

    def compile(self, globs, log):
        for f in safeglob_multi(globs):
            try:
                os.chmod(f, 0644)
            except Exception, e:
                log.err += "Error chmoding %s - %s\n" % (f, e)
        log.out += self.language + " scripts do not need to be compiled.\n"
        return True

class ExternalCompiler(Compiler):
    def __init__(self, args, separate=False):
        self.args = args
        self.separate = separate

    def compile(self, globs, log):
        files = safeglob_multi(globs)
        if self.separate:
            for file in files:
                if not system(self.args + [file], log):
                    return False
        else:
            if not system(self.args + files, log):
                return False
        return True

class JavaCompiler(ExternalCompiler):
    @staticmethod
    def safeglob(pattern):
        safepaths = []
        for root, dirs, files in os.walk("."):
            files = fnmatch.filter(files, pattern)
            for fname in files:
                if SAFEPATH.match(fname):
                    safepaths.append(os.path.join(root, fname))
        return safepaths

    @staticmethod
    def safeglob_multi(patterns):
        safepaths = []
        for pattern in patterns:
            safepaths.extend(JavaCompiler.safeglob(pattern))
        return safepaths

    @staticmethod
    def nukeglob(pattern):
        paths = JavaCompiler.safeglob(pattern)
        for path in paths:
            # Ought to be all files, not folders
            try:
                os.unlink(path)
            except OSError, e:
                if e.errno != errno.ENOENT:
                    raise

    def compile(self, globs, log):
        files = JavaCompiler.safeglob_multi(globs)
        if self.separate:
            for file in files:
                if not system(self.args + [file], log):
                    return False
        else:
            if not system(self.args + files, log):
                return False
        return True

# Compiles each file to its own output, based on the replacements dict.
class TargetCompiler(Compiler):
    def __init__(self, args, replacements, outflag="-o"):
        self.args = args
        self.replacements = replacements
        self.outflag = outflag

    def compile(self, globs, log):
        sources = safeglob_multi(globs)
        for source in sources:
            head, ext = os.path.splitext(source)
            if ext in self.replacements:
                target = head + self.replacements[ext]
            else:
                log.err += "Could not determine target for source file %s.\n" % source
                return False
            if not system(self.args + [self.outflag, target, source], log):
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
    "Go"            : [["/usr/local/bin/8g", "-o", "_go_.8"],
                             ["/usr/local/bin/8l", "-o", BOT, "_go_.8"]],
    "Groovy"    : [["groovyc"],
                             ["jar", "cfe", BOT + ".jar", BOT]],
    "Haskell" : [["ghc", "--make", BOT + ".hs", "-O", "-v0"]],
    "Java"        : [["javac"],
                             ["jar", "cfe", BOT + ".jar", BOT]],
    "Lisp"        : [['sbcl', '--end-runtime-options', '--no-sysinit',
                                '--no-userinit', '--disable-debugger', '--load',
                                BOT + '.lisp', '--eval', "(save-lisp-and-die \"" + BOT
                                + "\" :executable t :toplevel #'pwbot::main)"]],
    "OCaml"     : [["ocamlbuild", BOT + ".native"]],
    "Scala"     : [["scalac"]],
    }

targets = {
    # lang : { old_ext : new_ext, ... }
    "C"     : { ".c" : ".o" },
    "C++" : { ".c" : ".o", ".cpp" : ".o", ".cc" : ".o" },
    }

languages = {
    # lang : (output extension, [nukeglobs], [(source glob, compiler), ...])
    #                The compilers are run in the order given.
    #                If the extension is "" it means the output file is just BOT
    #                If a source glob is "" it means the source is part of the
    #                    compiler arguments.
    "C"                     : ("",
                                     ["*.o", BOT],
                                     [(["*.c"], TargetCompiler(comp_args["C"][0], targets["C"])),
                                        (["*.o"], ExternalCompiler(comp_args["C"][1]))]),
    "C#"                    : (".exe",
                                     [BOT + ".exe"],
                                     [(["*.cs"], ExternalCompiler(comp_args["C#"][0]))]),
    "C++"                 : ("",
                                     ["*.o", BOT],
                                     [(["*.c", "*.cpp", "*.cc"],
                                         TargetCompiler(comp_args["C++"][0], targets["C++"])),
                                        (["*.o"], ExternalCompiler(comp_args["C++"][1]))]),
    "Clojure"         : (".clj",
                                     [],
                                     [(["*.clj"], ChmodCompiler("Clojure"))]),
    "CoffeeScript": (".coffee",
                                     [],
                                     [(["*.coffee"], ChmodCompiler("CoffeeScript"))]),
    "Go"                    : ("",
                                     ["*.8", BOT],
                                     [(["*.go"], ExternalCompiler(comp_args["Go"][0])),
                                        ([""], ExternalCompiler(comp_args["Go"][1]))]),
    "Groovy"            : (".jar",
                                     ["*.class, *.jar"],
                                     [(["*.groovy"], ExternalCompiler(comp_args["Groovy"][0])),
                                        (["*.class"], ExternalCompiler(comp_args["Groovy"][1]))]),
    "Haskell"         : ("",
                                     [BOT],
                                     [([""], ExternalCompiler(comp_args["Haskell"][0]))]),
    "Java"                : (".jar",
                                     ["*.class", "*.jar"],
                                     [(["*.java"], JavaCompiler(comp_args["Java"][0])),
                                        (["*.class"], JavaCompiler(comp_args["Java"][1]))]),
    "Javascript"    : (".js",
                                     [],
                                     [(["*.js"], ChmodCompiler("Javascript"))]),
    "Lisp"                : ("",
                                     [BOT],
                                     [([""], ExternalCompiler(comp_args["Lisp"][0]))]),
    "Lua"                 : (".lua",
                                     [],
                                     [(["*.lua"], ChmodCompiler("Lua"))]),
    "OCaml"             : (".native",
                                     [BOT + ".native"],
                                     [([""], ExternalCompiler(comp_args["OCaml"][0]))]),
    "Perl"                : (".pl",
                                     [],
                                     [(["*.pl"], ChmodCompiler("Perl"))]),
    "PHP"                 : (".php",
                                     [],
                                     [(["*.php"], ChmodCompiler("PHP"))]),
    "Python"            : (".py",
                                     ["*.pyc"],
                                     [(["*.py"], ChmodCompiler("Python"))]),
    "Ruby"                : (".rb",
                                     [],
                                     [(["*.rb"], ChmodCompiler("Ruby"))]),
    "Scala"             : (".class",
                                     ["*.class, *.jar"],
                                     [(["*.scala"], ExternalCompiler(comp_args["Scala"][0]))]),
    "Scheme"            : (".ss",
                                     [],
                                     [(["*.ss"], ChmodCompiler("Scheme"))]),
    }


def compile_function(language, log):
    info = languages[language]
    extension = info[0]
    nukeglobs = info[1]
    compilers = info[2]

    if language == "Java":
        for glob in nukeglobs:
            JavaCompiler.nukeglob(glob)
    else:
        for glob in nukeglobs:
            nukeglob(glob)

    for globs, compiler in compilers:
        if not compiler.compile(globs, log):
            return False

    return check_path(BOT + extension, log)

def get_programming_languages():
    connection = MySQLdb.connect(host = server_info["db_host"],
                                 user = server_info["db_username"],
                                 passwd = server_info["db_password"],
                                 db = server_info["db_name"])
    cursor = connection.cursor(MySQLdb.cursors.DictCursor)
    cursor.execute("SELECT * FROM language where language_id > 0")
    programming_languages = cursor.fetchall()
    cursor.close()
    connection.close()
    return programming_languages

def detect_language(programming_languages):
    # Autodetects the language of the entry in the current working directory
    log = Log()
    if programming_languages == None:
        programming_languages = get_programming_languages()
    detected_langs = [
        lang for lang in programming_languages \
            if os.path.exists(lang["main_code_file"])
    ]
    print os.listdir(os.getcwd())
    print detected_langs
    # If no language was detected
    if len(detected_langs) == 0:
        log.err += "The auto-compile environment could not locate your main code\n"
        log.err += "file. This is probably because you accidentally changed the\n"
        log.err += "name of your main code file. You must include exactly one file"
        log.err += "\nwith one of the following names:\n"
        for lang in programming_languages:
            log.err += "     * " + lang["main_code_file"] + " (" + lang["name"] + ")\n"
        log.err += "This is to help the auto-compile environment figure out which"
        log.err += "\nprogramming language you are using.\n"
        return log.out, log.err, "NULL"
    # If more than one language was detected
    if len(detected_langs) > 1:
        log.err = "The auto-compile environment found more than one main code "
        log.err += "file:\n"
        for lang in detected_langs:
            log.err += "     * " + lang["main_code_file"] + " (" + lang["name"] + ")\n"
        log.err += "You must submit only one of these files so that the\n"
        log.err += "auto-compile environment can figure out which programming\n"
        log.err += "language you are trying to use.\n"
        return log.out, log.err, "NULL"
    return detected_langs[0]

# Autodetects the language of the entry in the current working directory and
# compiles it.
def compile_anything(programming_languages=None):
    log = Log()
    # If we get this far, then we have successfully auto-detected the language
    # that this contestant is using.
    detected_language = detect_language(programming_languages)
    main_code_file = detected_language["main_code_file"]
    detected_lang = detected_language["name"]
    language_id = detected_language["language_id"]
    log.out += "Found " + main_code_file + ". Compiling this entry as " + \
        detected_lang + "\n"
    t1 = time.time()
    if compile_function(detected_lang, log):
        log.out += "Completed in %f seconds.\n" % (time.time() - t1)
    else:
        log.err += "Compilation failed.\n"
    return log.out, log.err, language_id

if __name__ == '__main__':
    import json
    out, err, language_id = compile_anything()
    print json.dumps((out, err, language_id))

