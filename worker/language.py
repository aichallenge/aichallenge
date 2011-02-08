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

SAFEPATH = re.compile('[a-zA-Z0-9_.$-]+$')

BOT = "MyBot"

class CompileAction(object):
    """ An action to be taken on a submission. The __call__ function
        is what the compiler will use to invoke the action, sending
        two arguments: first, the list of files to perform the action on;
        and second the submission object itself. """
    def __init__(self, sourceglobs=(), deepglobs=()):
        """ Minimally, a CompileAction consists of
            sourceglobs - a list of glob syntax strings which describe
                          the files upon which the action is taken.
            deepglobs - similar to sourceglobs, but we grab all files
                        in subdirectories that match these globs, instead
                        of only our present directory as with sourceglobs.
            These both can be empty; this is often the case when we
            uses one or more specific source files as an argument. """
        self.sourceglobs = sourceglobs
        self.deepglobs = deepglobs

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
    def __init__(self, sourceglobs=(), deepglobs=(), args=()):
        """ args - the list of string arguments for this commandline action.

            If any argument is "{file}", the commandline is run individually
            for each source file given, with the "{file}" argument replaced
            with the name of the file.

            Otherwise, the commandline is run once, appending all
            source files to the end of the argument list. """
        self.sourceglobs = sourceglobs
        self.deepglobs = deepglobs
        self.args = args

    def __call__(self, sources, subm):
        if "{file}" in self.args:
            success = True
            for source in sources:
                cmd = [arg == "{file}" and repr(source) or arg
                       for arg in self.args]
                subm.compile_output += ' '.join(cmd) + '\n'
                proc = subprocess.Popen(cmd, stdout=subprocess.PIPE,
                                        stderr=subprocess.PIPE)
                # Attach the process so an external timer can kill it.
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


# The master list of languages!
languages = {
    # Each language is a dict with some of the following keys:
    # main_ext, output_ext, nuke_globs, compile, run.
    # main_ext - The main source file is always BOT + this.
    #            If a list or tuple, the main file can use any of the options.
    # output_ext - The executable/interpretable is always BOT + this.
    #              If not present, assumed to be "" if compile exists, or
    #              the main source file if compile does not exist. Must be
    #              present if main_ext is a list.
    # nuke_globs - A list of globs that are removed before compilation.
    #              If not present, assumed to be empty.
    # compile - A list of CompileActions to be performed on source files.
    #           If not present, the compilation will perform a Chmod on all
    #           source files that end in main_ext.
    # run - A string giving the command to run a bot in this language.
    #       Should include up to one "%s" in which the path to
    #       the executable/interpretable will be put. If the "%s" is omitted,
    #       the command is used as given. If this key is not present, assumed
    #       to be "%s" (i.e. the executable 'BOT').
    # disabled - If present, disables the language from being used.
    "C": {
        "main_ext": ".c",
        "nuke_globs": ["*.o", BOT],
        "compile": [
            System(sourceglobs=["*.c"],
                   args=["gcc", "-O3", "-c", "{file}"]),
            System(sourceglobs=["*.o"],
                   args=["gcc", "-O2", "-lm", "-o", BOT]),
        ],
    },

    "C#": {
        "main_ext": ".cs",
        "output_ext": ".exe",
        "nuke_globs": [BOT + ".exe"],
        "compile": [
            System(sourceglobs=["*.cs"],
                   args=["gmcs", "-warn:0", "-out:%s.exe" % BOT]),
        ],
        "run": "mono %s",
    },

    "C++": {
        "main_ext": [".cc", ".cpp", ".cxx"],
        "nuke_globs": ["*.o", BOT],
        "compile": [
            System(sourceglobs=["*.c", "*.cc", "*.cpp", "*.cxx"],
                   args=["g++", "-O3", "-c", "{file}"]),
            System(sourceglobs=["*.o"],
                   args=["g++", "-O2", "-lm", "-o", BOT]),
        ],
    },

    "Clojure": {
        "main_ext": ".clj",
        "run": "clojure %s",
        "disabled": True,
    },

    "CoffeeScript": {
        "main_ext": ".coffee",
        "run": "coffee %s",
    },

    "Go": {
        "main_ext": ".go",
        "nuke_globs": ["*.8", BOT],
        "compile": [
            System(sourceglobs=["*.go"],
                   args=["8g", "-o", "_go_.8"]),
            System(sourceglobs=[],
                   args=["8l", "-o", BOT, "_go_.8"]),
        ],
    },

    "Groovy": {
        "main_ext": ".groovy",
        "output_ext": ".jar",
        "nuke_globs": ["*.class", "*.jar"],
        "compile": [
            System(sourceglobs=["*.groovy"],
                   args=["groovyc"]),
            System(sourceglobs=["*.class"],
                   args=["jar", "cfe", BOT + ".jar", BOT]),
        ],
        "run": "java -cp %s:/usr/share/groovy/"
               "embeddable/groovy-all-1.7.5.jar " + BOT,
    },

    "Haskell": {
        "main_ext": ".hs",
        "nuke_globs": [BOT],
        "compile": [
            System(args=["ghc", "--make", BOT + ".hs", "-O2", "-v0"]),
        ],
    },

    "Java": {
        "main_ext": ".java",
        "output_ext": ".jar",
        "nuke_globs": ["*.class", "*.jar"],
        "compile": [
            System(deepglobs=["*.java"],
                   args=["javac"]),
            System(deepglobs=["*.class"],
                   args=["jar", "cfe", BOT + ".jar", BOT]),
        ],
        "run": "java -jar %s",
    },

    "Javascript": {
        "main_ext": ".js",
        "run": "node %s",
    },

    "Lisp": {
        "main_ext": ".lisp",
        "nuke_globs": [BOT],
        "compile": [
            System(args=["sbcl", "--end-runtime-options", "--no-sysinit",
                         "--no-userinit", "--disable-debugger", "--load",
                         BOT + ".lisp", "--eval",
                         ('(save-lisp-and-die "%s" :executable t '
                          ":toplevel #'pwbot::main)") % BOT]),
        ],
    },

    "Lua": {
        "main_ext": ".lua",
        "run": "lua %s",
        "disabled": True,
    },

    "OCaml": {
        "main_ext": ".ml",
        "output_ext": ".native",
        "nuke_globs": [BOT + ".native"],
        "compile": [
            System(args=["ocamlbuild", BOT + ".native"]),
        ],
    },

    "Perl": {
        "main_ext": ".pl",
        "run": "perl %s",
    },

    "PHP": {
        "main_ext": ".php",
        "run": "php %s",
    },

    "Python": {
        "main_ext": ".py",
        "nuke_globs": ["*.pyc"],
        "run": "python %s",
    },

    "Ruby": {
        "main_ext": ".rb",
        "run": "ruby %s",
    },

    "Scala": {
        "main_ext": ".scala",
        "output_ext": ".class",
        "nuke_globs": ["*.class", "*.jar"],
        "compile": [
            System(sourceglobs=["*.scala"],
                   args=["scalac"]),
        ],
        # needs the classname, not the name of the file
        "run": "scala " + BOT,
    },

    "Scheme": {
        "main_ext": ".ss",
        # This is highly dependent on which Scheme interpreter is used.
        "run": "scheme %s",
        "disabled": True,
    },
}
for lg in languages:
    if type(languages[lg]["main_ext"]) in (list, tuple):
        languages[lg]["main_file"] = [BOT + ext
                                      for ext in languages[lg]["main_ext"]]
    else:
        languages[lg]["main_file"] = BOT + languages[lg]["main_ext"]


class Compiler(object):
    """ Stub base class for compilers. """
    def __init__(self, output_file, nukeglobs, actions):
        """ Every compiler has minimally the following:

            output_file - the file of the executable or
                          interpretable, eg. 'MyBot.py' or just 'MyBot'.
            nukeglobs - a list of glob syntax strings which describe
                        the files to be removed before compilation.
            actions - a list of CompileActions to be performed on the
                      submission. """
        self.output_file = output_file
        self.nukeglobs = nukeglobs
        self.actions = actions
        self.use_deepglob = any(action.deepglobs for action in actions)

    def compile(self, subm):
        """ Attempt to compile the given submission. Any output or errors
            produced will be logged to the submission's compile_output or
            compile_errors values, respectively. """
        for pattern in self.nukeglobs:
            self.nukeglob(pattern)
        for action in self.actions:
            sources = (self.safeglob_multi(action.sourceglobs)
                       + self.deepglob_multi(action.deepglobs))
            if not action(sources, subm):
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

    @staticmethod
    def deepglob(pattern):
        """ Grabs all files in the current directory and any subdirectories.
            This is used primarily for languages that support arranging their
            source files in package hierarchies. """
        safepaths = []
        for root, dirs, files in os.walk("."):
            safepaths.extend([os.path.join(root, fname)
                              for fname in fnmatch.filter(files, pattern)
                              if SAFEPATH.match(fname)])
        return safepaths

    @classmethod
    def deepglob_multi(cls, patterns):
        return list(itertools.chain([cls.deepglob(pat) for pat in patterns]))

    def nukeglob(self, pattern):
        paths = (self.use_deepglob and self.deepglob(pattern)
                 or self.safeglob(pattern))
        for path in paths:
            try:
                if os.path.isdir(path):
                    shutil.rmtree(path)
                else:
                    os.unlink(path)
            except OSError as e:
                if e.errno != ENOENT:
                    raise

        
def compile_submission(subm, max_time=None):
    """ Determines which language the given submission is coded in, and
        compiles it. Optionally, a time limit may be specified to prevent
        overlong compilation times. """
    def detect_languages():
        for lg in languages:
            if not languages[lg].get("disabled"):
                if type(languages[lg]["main_file"]) in (list, tuple):
                    for main_file in languages[lg]["main_file"]:
                        if os.path.exists(main_file):
                            yield (lg, main_file)
                else:
                    if os.path.exists(languages[lg]["main_file"]):
                        yield (lg, languages[lg]["main_file"])
    detected_languages = [(lg, mf) for (lg, mf) in detect_languages()]
    if len(detected_languages) == 0:
        subm.compile_errors += (
            "The auto-compile environment could not locate your main code "
            "file. This is probably because you accidentally changed the "
            "name of your main code file. Please check the languages list "
            "for the file you must include so that the auto-compile "
            "environment can figure out which language you are using.\n")
        return False
    elif len(detected_languages) > 1:
        subm.compile_errors += (
            "The auto-compile environment found more than one main code "
            "file:\n" + "\n".join(["  * %s (%s)" % (mf, lg)
                                   for lg, mf in detected_languages])
            + "\nYou must submit only one of these files so that the "
            "auto-compile environment can figure out which "
            "language you are using.\n")
        return False
    else:
        lg, mf = detected_languages[0]
        lgdict = languages[lg]
        subm.language = lg
        subm.compile_output += ("Found " + mf + ". Compiling "
                                "this entry as " + lg + ".\n")
        if "compile" in lgdict:
            subm.bot = BOT + lgdict.get("output_ext", "")
            actions = lgdict["compile"]
        else:
            if type(lgdict["main_ext"]) in (list, tuple):
                sourceglobs=['*' + ext for ext in lgdict["main_ext"]]
            else:
                sourceglobs=['*' + lgdict["main_ext"]]
            chmod = Chmod(sourceglobs=sourceglobs)
            # If output_ext doesn't exist, main_ext is a string
            subm.bot = BOT + lgdict.get("output_ext", lgdict["main_ext"])
            actions = [chmod]
        compiler = Compiler(subm.bot,
                            lgdict.get("nuke_globs", ()),
                            actions)
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
            return False
        if success:
            subm.compile_output += ("Completed in %f seconds.\n"
                                    % (time.time() - t1))
            return True
        else:
            subm.compile_errors += "Compilation failed.\n"
            return False

def get_command(subm):
    """ Return the command used to run the given submission. """
    cmd = languages[subm.language].get("run", "%s/" + BOT)
    if "%s" in cmd:
        cmd = cmd % (subm.directory + '/' + subm.bot)
    return cmd
