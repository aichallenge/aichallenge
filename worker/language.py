
BOT = "MyBot"

# This is simultaneously a master list of the available languages
# as well as a mapping of language to the shell command to run a bot
# in that language.
commands = {
    "C":            "./" + BOT,
    "C#":           "mono %s.exe" % BOT,
    "C++":          "./" + BOT,
    "CoffeeScript": "coffee %s.coffee" % BOT,
    "Go":           "./" + BOT,
    "Groovy":       ("java -cp %s.jar:/usr/share/groovy/"
                     "embeddable/groovy-all-1.7.5.jar %s" % (BOT, BOT)),
    "Haskell":      "./" + BOT,
    "Java":         "java -jar %s.jar" % BOT,
    "Javascript":   "node %s.js" % BOT,
    "Lisp":         "./" + BOT,
    "OCaml":        "./%s.native" % BOT,
    "Perl":         "perl %s.pl" % BOT,
    "PHP":          "php %s.php" % BOT,
    "Python":       "python %s.py" % BOT,
    "Ruby":         "ruby %s.rb" % BOT,
}

def is_valid_language(lang):
    return lang in commands

def get_command(lang):
    """ Returns None if lang is not a valid language. """
    return commands.get(lang)
