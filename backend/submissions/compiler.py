from errno import ENOENT
import fnmatch
import glob
import itertools
import os
import re
import shutil
import signal
import subprocess
import time
from submission import *

BOT = "MyBot"
SAFEPATH = re.compile('[a-zA-Z0-9_.$-]+$')

class Compiler(object):
    """ Stub base class for compilers. """
    def __init__(self, language, main_file, output_file, nukeglobs, *actions):
        """ Every language has minimally the following:

            language - its name.
            main_file - the name of the main code file, eg. BOT + '.py'.
            output_file - the file of the executable or
                          interpretable, eg BOT + '.py' or just BOT.
            nukeglobs - a list of glob syntax strings which describe
                        the files to be removed before compilation.
            actions - a list of CompileActions to be performed on the
                      submission. """
        self.language = language
        self.main_file = main_file
        self.output_file = output_file
        self.nukeglobs = nukeglobs
        self.actions = actions

    def compile(self, subm):
        """ Attempt to compile the given submission. Any output or errors
            produced will be logged to the submission's compile_output or
            compile_errors values, respectively. """
        for pattern in self.nukeglobs:
            self.nukeglob(pattern)
        for action in self.actions:
            if not action(self.safeglob_multi(action.sourceglobs), subm):
                return False
        if not os.path.exists(self.output_file):
            subm.compile_errors += ("\nFailure: output file "
                                    + self.output_file + " was not created.\n")
            return False
        return True

    @staticmethod
    def safeglob(pattern):
        return [path for path in glob.glob(pattern) if SAFEPATH.match(path)]

    @classmethod
    def safeglob_multi(cls, patterns):
        return list(itertools.chain([cls.safeglob(pat) for pat in patterns]))

    @classmethod
    def nukeglob(cls, pattern):
        paths = cls.safeglob(pattern)
        for path in paths:
            try:
                if os.path.isdir(path):
                    shutil.rmtree(path)
                else:
                    os.unlink(path)
            except OSError as e:
                if e.errno != ENOENT:
                    raise

class PackageCompiler(Compiler):
    """ A compiler that will grab all files in the current directory and
        any subdirectories, in order to compile each of them. This is used
        primarily for languages that support arranging their source files
        in package hierarchies. """
    @staticmethod
    def safeglob(pattern):
        safepaths = []
        for root, dirs, files in os.walk("."):
            safepaths.extend([os.path.join(root, fname)
                              for fname in fnmatch.filter(files, pattern)
                              if SAFEPATH.match(fname)])
        return safepaths

class CompileAction(object):
    """ An action to be taken on a submission. The __call__ function
        is what the compiler will use to invoke the action, sending
        two arguments: first, the list of files to perform the action on;
        and second the submission object itself. """
    def __init__(self, sourceglobs):
        """ Minimally, a CompileAction consists of
            sourceglobs - a list of glob syntax strings which describe
                          the files upon which the action is taken.
            It can be empty; this is often used when the System action
            uses one or more specific source files as an argument. """
        self.sourceglobs = sourceglobs

class Chmod(CompileAction):
    """ For languages that are interpreted rather than compiled,
        we simply chmod their files to ensure they are there. """
    def __call__(self, sources, subm):
        for source in sources:
            try:
                os.chmod(source, 0644)
            except Exception as e:
                subm.compile_errors += "Error chmoding %s - %s\n" % (source, e)
        subm.compile_output += "These scripts don't need to be compiled.\n"
        return True
        
class System(CompileAction):
    """ An action that runs a given commandline argument. """
    def __init__(self, sourceglobs, args):
        """ args - the list of string arguments for this commandline action.

            If any argument is "{file}", the commandline is run individually
            for each source file given, with the "{file}" argument replaced
            with the name of the file.

            If any argument contains "{head}", the commandline is run
            individually for each source file given, with the "{head}"
            argument replaced with the name of the file without its
            extension. (e.g. "Bot.py" => "Bot")

            Otherwise, the commandline is run once, appending all
            source files to the end of the argument list. """
        self.sourceglobs = sourceglobs
        self.args = args

    def __call__(self, sources, subm):
        if "{file}" in self.args or any(("{head}" in a for a in self.args)):
            success = True
            for source in sources:
                head, ext = os.path.splitext(source)
                cmd = [ arg == "{file}" and repr(source)
                        or "{head}" in arg and repr(arg.format(head=head))
                        or arg for arg in self.args ]
                subm.compile_output += ' '.join(cmd) + '\n'
                proc = subprocess.Popen(cmd, stdout=subprocess.PIPE,
                                        stderr=subprocess.PIPE)
                subm._proc = proc
                out, err = proc.communicate()
                del subm._proc
                subm.compile_output += out
                subm.compile_errors += err
                if proc.returncode != 0:
                    success = False
                    # But continue this action on all files anyway
                    # to generate as many errors to hand back to the user
            return success
        else:
            cmd = self.args + [repr(s) for s in sources]
            subm.compile_output += ' '.join(cmd) + '\n'
            proc = subprocess.Popen(cmd, stdout=subprocess.PIPE,
                                    stderr=subprocess.PIPE)
            subm._proc = proc
            out, err = proc.communicate()
            del subm._proc
            subm.compile_output += out
            subm.compile_errors += err
            return proc.returncode == 0

compilers = [
        Compiler("C", BOT + ".c", BOT, ["*.o", BOT],
                 System(sourceglobs=["*.c"],
                        args=["gcc", "-O3", "-c", "{file}", "-o", "{head}.o"]),
                 System(sourceglobs=["*.o"],
                        args=["gcc", "-O2", "-lm", "-o", BOT])),
        Compiler("C#", BOT + ".cs", BOT + ".exe", [BOT + ".exe"],
                 System(sourceglobs=["*.cs"],
                        args=["gmcs", "-warn:0", "-out:%s.exe" % BOT])),
        Compiler("C++", BOT + ".cc", BOT, ["*.o", BOT],
                 System(sourceglobs=["*.c", "*.cc", "*.cpp"],
                        args=["g++", "-O3", "-c", "{file}", "-o", "{head}.o"]),
                 System(sourceglobs=["*.o"],
                        args=["g++", "-O2", "-lm", "-o", BOT])),
        Compiler("Clojure", BOT + ".clj", BOT + ".clj", [],
                 Chmod(["*.clj"])),
        Compiler("CoffeeScript", BOT + ".coffee", BOT + ".coffee", [],
                 Chmod(["*.coffee"])),
        Compiler("Go", BOT + ".go", BOT, ["*.8", BOT],
                 System(sourceglobs=["*.go"],
                        args=["8g", "-o", "_go_.8"]),
                 System(sourceglobs=[],
                        args=["8l", "-o", BOT, "_go_.8"])),
        Compiler("Groovy", BOT + ".groovy", BOT + ".jar", ["*.class", "*.jar"],
                 System(sourceglobs=["*.groovy"],
                        args=["groovyc"]),
                 System(sourceglobs=["*.class"],
                        args=["jar", "cfe", BOT + ".jar", BOT])),
        Compiler("Haskell", BOT + ".hs", BOT, [BOT],
                 System(sourceglobs=[],
                        args=["ghc", "--make", BOT + ".hs", "-O2", "-v0"])),
        PackageCompiler("Java", BOT + ".java", BOT + ".jar",
                        ["*.class", "*.jar"],
                        System(sourceglobs=["*.java"],
                               args=["javac"]),
                        System(sourceglobs=["*.class"],
                               args=["jar", "cfe", BOT + ".jar", BOT])),
        Compiler("Javascript", BOT + ".js", BOT + ".js", [],
                 Chmod(["*.js"])),
        Compiler("Lisp", BOT + ".lisp", BOT, [BOT],
                 System(sourceglobs=[],
                        args=["sbcl", "--end-runtime-options", "--no-sysinit",
                              "--no-userinit", "--disable-debugger", "--load",
                              BOT + ".lisp", "--eval",
                              ('(save-lisp-and-die "%s" :executable t '
                               ":toplevel #'pwbot::main)") % BOT])),
        Compiler("Lua", BOT + ".lua", BOT + ".lua", [],
                 Chmod(["*.lua"])),
        Compiler("OCaml", BOT + ".ml", BOT + ".native", [BOT + ".native"],
                 System(sourceglobs=[],
                        args=["ocamlbuild", BOT + ".native"])),
        Compiler("Perl", BOT + ".pl", BOT + ".pl", [],
                 Chmod(["*.pl"])),
        Compiler("PHP", BOT + ".php", BOT + ".php", [],
                 Chmod(["*.php"])),
        Compiler("Python", BOT + ".py", BOT + ".py", ["*.pyc"],
                 Chmod(["*.py"])),
        Compiler("Ruby", BOT + ".rb", BOT + ".rb", [],
                 Chmod(["*.rb"])),
        Compiler("Scala", BOT + ".scala", BOT + ".class", ["*.class", "*.jar"],
                 System(sourceglobs=["*.scala"],
                        args=["scalac"])),
        Compiler("Scheme", BOT + ".ss", BOT + ".ss", [],
                 Chmod(["*.ss"])),
]

def compile_submission(subm, max_time=None):
    """ Determines which language the given submission is coded in, and
        compiles it. Optionally, a time limit may be specified to prevent
        overlong compilation times. """
    assert subm.status == UNCOMPILED # don't attempt multiple compilations
    detected_compilers = [compiler for compiler in compilers
                          if os.path.exists(compiler.main_file)]
    if len(detected_compilers) == 0:
        subm.compile_errors += (
             "The auto-compile environment could not locate your main code "
             "file. This is probably because you accidentally changed the "
             "name of your main code file. Please check the languages list "
             "for the file you must include so that the auto-compile "
             "environment can figure out which language you are using.\n")
        return False
    elif len(detected_compilers) > 1:
        subm.compile_errors += (
             "The auto-compile environment found more than one main code "
             "file:\n" + "\n".join(["  * %s (%s)" % (c.main_file, c.language)
                                    for c in detected_compilers])
             + "\nYou must submit only one of these files so that the auto-"
             "compile environment can figure out which "
             "language you are using.\n")
        return False
    else:
        compiler = detected_compilers[0]
        subm.compile_output += ("Found " + compiler.main_file + ". Compiling "
                                "this entry as " + compiler.language + ".\n")
        subm.language = compiler.language
        def stop_compiling(signum, frame):
            signal.signal(signal.SIGALRM, signal.SIG_DFL)
            if hasattr(subm, "_proc"):
                os.kill(subm._proc.pid, signal.SIGKILL)
            raise Exception("Compilation did not finish in %f seconds."
                            % max_time)
        signal.signal(signal.SIGALRM, stop_compiling)
        t1 = time.time()
        try:
            signal.alarm(max_time)
            success = compiler.compile(subm)
            signal.alarm(0)
            signal.signal(signal.SIGALRM, signal.SIG_DFL)
        except Exception as e:
            subm.compile_errors += str(e) + "\n"
            subm.status = COMPILE_FAILED
            return False
        if success:
            subm.compile_output += ("Completed in %f seconds.\n"
                                    % (time.time() - t1))
            subm.status = UNTESTED # compile success
            return True
        else:
            subm.compile_errors += "Compilation failed.\n"
            subm.status = COMPILE_FAILED
            return False
