from errno import ENOENT
import fnmatch
import glob
import itertools
import os
import re
import shlex
import shutil
import signal
import subprocess
import time
import traceback

from worker.runner import Runner, TimeoutError

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
    def __call__(self, sources, subm, runner):
        for source in sources:
            try:
                os.chmod(source, 0644)
            except Exception as e:
                subm.compile_errors += "Error chmoding %s - %s\n" % (source, e)
                return False
        subm.compile_output += "These scripts don't need to be compiled.\n"
        return True

class System(CompileAction):
    """ An action that runs a given commandline argument. """
    def __init__(self, args=(), sourceglobs=(), deepglobs=()):
        """ args - the list of string arguments for this commandline action.

            If any argument is "{file}", the commandline is run individually
            for each source file given, with the "{file}" argument replaced
            with the name of the file.

            Otherwise, the commandline is run once, appending all
            source files to the end of the argument list. """
        CompileAction.__init__(self, sourceglobs, deepglobs)
        if isinstance(args, str):
            args = shlex.split(args)
        self.args = args

    def __call__(self, sources, subm, runner):
        if "{file}" in self.args:
            success = True
            for source in sources:
                cmd = [arg.format(file=source, bot=BOT, path=os.getcwd())
                        for arg in self.args]
                subm.compile_output += ' '.join(cmd) + '\n'
                runner.run(cmd, should_stop=False)
                out, err = runner.process.communicate()
                subm.compile_output += out
                subm.compile_errors += err
                if runner.process.returncode != 0:
                    success = False
            return success
        else:
            cmd = [arg.format(bot=BOT, path=os.getcwd())
                    for arg in self.args] + sources
            subm.compile_output += ' '.join(cmd) + '\n'
            runner.run(cmd, should_stop=False)
            out, err = runner.process.communicate()
            subm.compile_output += out
            subm.compile_errors += err
            return runner.process.returncode == 0

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
    #       If omitted, it is assumbled that a binary named by the constant
    #       BOT should be run (the default value is "{path}/{file}")
    # disabled - If present, prevents the language from being used.
    #
    # The following substitutions will be made:
    #   - {bot} will be substituted for the constant BOT in both the
    #     System compile command and the run command
    #   - {file} will be replaced with each individual file matched by the glob
    #   - {path} will be replaced with the path from which the submission is
    #     being run or compiled
    "C": {
        "main_ext": ".c",
        "nuke_globs": ["*.o", BOT],
        "compile": [
            System("gcc -O3 -c {file}", sourceglobs=["*.c"]),
            System("gcc -O2 -lm -o {bot}", sourceglobs=["*.o"]),
        ],
    },

    "C#": {
        "main_ext": ".cs",
        "output_ext": ".exe",
        "nuke_globs": [BOT + ".exe"],
        "compile": [
            System("gmcs -warn:0 -out:{bot}", sourceglobs=["*.cs"]),
        ],
        "run": "mono {bot}.exe",
    },

    "C++": {
        "main_ext": [".cc", ".cpp", ".cxx"],
        "nuke_globs": ["*.o", BOT],
        "compile": [
            System("g++ -O3 -c {file}",
                   sourceglobs=["*.c", "*.cc", "*.cpp", "*.cxx"]),
            System("g++ -O2 -lm -o {bot}", sourceglobs=["*.o"]),
        ],
    },

    "Clojure": {
        "main_ext": ".clj",
        "run": "clojure {bot}.clj",
        "disabled": True,
    },

    "CoffeeScript": {
        "main_ext": ".coffee",
        "run": "coffee {bot}.coffee",
    },

    "Go": {
        "main_ext": ".go",
        "nuke_globs": ["*.8", BOT],
        "compile": [
            System("8g -o _go_.8", sourceglobs=["*.go"]),
            System("8l -o {bot} _go_.8"),
        ],
    },

    "Groovy": {
        "main_ext": ".groovy",
        "output_ext": ".jar",
        "nuke_globs": ["*.class", "*.jar"],
        "compile": [
            System("groovyc", sourceglobs=["*.groovy"]),
            System("jar cfe {bot}.jar {bot}", sourceglobs=["*.class"]),
        ],
        "run": "java -cp {bot}.jar:/usr/share/groovy/"
               "embeddable/groovy-all-1.7.5.jar {bot}",
    },

    "Haskell": {
        "main_ext": ".hs",
        "nuke_globs": [BOT],
        "compile": [
            System("ghc --make {bot}.hs -O2 -v0"),
        ],
    },

    "Java": {
        "main_ext": ".java",
        "output_ext": ".jar",
        "nuke_globs": ["*.class", "*.jar"],
        "compile": [
            System("javac", deepglobs=["*.java"]),
            System("jar cfe {bot}.jar {bot}", deepglobs=["*.class"]),
        ],
        "run": "java -jar {bot}",
    },

    "Javascript": {
        "main_ext": ".js",
        "run": "node {bot}.js",
    },

    "Lisp": {
        "main_ext": ".lisp",
        "nuke_globs": [BOT],
        "compile": [
            System("sbcl --end-runtime-options --no-sysinit --no-userinit "
                         "--disable-debugger --load {bot}.lisp --eval "
                         '"(save-lisp-and-die \"{bot}\" :executable t '
                          ":toplevel #'pwbot::main)\""),
        ],
    },

    "Lua": {
        "main_ext": ".lua",
        "run": "lua {bot}.lua",
        "disabled": True,
    },

    "OCaml": {
        "main_ext": ".ml",
        "output_ext": ".native",
        "nuke_globs": [BOT + ".native"],
        "compile": [
            System("ocamlbuild {bot}.native"),
        ],
        "run": "{path}/{bot}.native"
    },

    "Perl": {
        "main_ext": ".pl",
        "run": "perl {bot}.pl",
    },

    "PHP": {
        "main_ext": ".php",
        "run": "php {bot}.pl",
    },

    "Python": {
        "main_ext": ".py",
        "nuke_globs": ["*.pyc"],
        "run": "python {bot}.py",
    },

    "Ruby": {
        "main_ext": ".rb",
        "run": "ruby1.9.1 {bot}.rb",
    },

    "Scala": {
        "main_ext": ".scala",
        "output_ext": ".class",
        "nuke_globs": ["*.class", "*.jar"],
        "compile": [
            System("scalac", sourceglobs=["*.scala"]),
        ],
        # needs the classname, not the name of the file
        "run": "scala {bot}",
    },

    "Scheme": {
        "main_ext": ".ss",
        # This is highly dependent on which Scheme interpreter is used.
        "run": "scheme {bot}.ss",
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
    def __init__(self, origin, output_file, nukeglobs, actions):
        """ Every compiler has minimally the following:
            origin - the location of the source files
            output_file - the file of the executable or
                          interpretable, eg. 'MyBot.py' or just 'MyBot'.
            nukeglobs - a list of glob syntax strings which describe
                        the files to be removed before compilation.
            actions - a list of CompileActions to be performed on the
                      submission. """
        self.origin = origin
        self.output_file = output_file
        self.nukeglobs = nukeglobs
        self.actions = actions
        self.use_deepglob = any(action.deepglobs for action in actions)
        self.runner = Runner(origin)

    def compile(self, subm):
        """ Attempt to compile the given submission. Any output or errors
            produced will be logged to the submission's compile_output or
            compile_errors values, respectively. """
        old_cwd = os.getcwd()
        os.chdir(self.runner.working)
        try:
            for pattern in self.nukeglobs:
                self.nukeglob(pattern)
            for action in self.actions:
                sources = (self.safeglob_multi(action.sourceglobs)
                           + self.deepglob_multi(action.deepglobs))
                if not action(sources, subm, self.runner):
                    return False
            if not os.path.exists(self.output_file):
                subm.compile_errors += ("\nFailure: output file "
                                    + self.output_file + " was not created.\n")
                return False
            return True
        finally:
            os.chdir(old_cwd)
    
    @staticmethod
    def safeglob(pattern):
        return [path for path in glob.glob(pattern) if SAFEPATH.match(path)]

    @classmethod
    def safeglob_multi(cls, patterns):
        return list(itertools.chain(*[cls.safeglob(pat) for pat in patterns]))

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
        return list(itertools.chain(*[cls.deepglob(pat) for pat in patterns]))

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

def detect_languages(subm):
    old_cwd = os.getcwd()
    os.chdir(subm.directory)
    try:
        for lg in languages:
            if languages[lg].get("disabled"): continue
            main_files = languages[lg]["main_file"]
            if type(main_files) not in (list, tuple):
                main_files = (main_files,)
            for main_file in main_files:
                if os.path.exists(main_file):
                    yield (lg, main_file)
    finally:
        os.chdir(old_cwd)

def compile_submission(subm, max_time=300):
    """ Determines which language the given submission is coded in, and
        compiles it. Optionally, a time limit may be specified to prevent
        overlong compilation times. """
    detected_languages = list(detect_languages(subm))
    
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
        message = "Found %s. Compiling this entry as %s.\n" % (mf, lg)
        subm.compile_output += message
        
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
        nuke_globs = lgdict.get("nuke_globs", ())
        
        t1 = time.time()
        success = False
        compiler = None
        try:
            compiler = Compiler(subm.directory, subm.bot, nuke_globs, actions)
            with compiler.runner.time_limit(max_time):
                success = compiler.compile(subm)
        except TimeoutError:
            subm.compile_errors += (
                "Compilation timed out after %.2f seconds.\n" % max_time)
            return False
        except Exception as e:
            subm.compile_errors += str(e) + "\n"
            subm.compile_errors += traceback.format_exc()
            return False
        finally:
            if success:
                # replace the origin directory
                tmp = "%s.%d.tmp" % (compiler.runner.origin, time.time())
                shutil.copytree(compiler.runner.working, tmp, True)
                shutil.rmtree(compiler.runner.origin)
                shutil.move(tmp, compiler.runner.origin)
                
                # tag the submission as compiled
                meta_dir = compiler.runner.origin + "/.aichallenge"
                if not os.path.exists(meta_dir):
                    os.mkdir(meta_dir)
                with open(meta_dir + "/compiled", 'w') as f:
                    f.write(time.asctime())
            if compiler is not None: compiler.runner.done()
        
        if success:
            subm.compile_output += (
                "Completed in %.2f seconds.\n" % (time.time() - t1))
            return True
        else:
            subm.compile_output += "Compilation failed.\n"
            return False

def get_command(subm, directory):
    """ Return the command used to run the given submission. """
    if subm.language not in languages: raise(ValueError, "unrecognized language")
    cmd = languages[subm.language].get("run", "{path}/{bot}")
    cmd = cmd.format(bot=BOT, path=directory)
    return cmd
