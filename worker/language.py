
BOT = "MyBot"

# This is simultaneously a master list of the available languages
# as well as a mapping of language to the shell command to run a bot
# in that language.
commands = {
    "C":            "%s/" + BOT,
    "C#":           "mono %%s/%s.exe" % BOT,
    "C++":          "%s/" + BOT,
    "CoffeeScript": "coffee %%s/%s.coffee" % BOT,
    "Go":           "%s/" + BOT,
    "Groovy":       ("java -cp %%s/%s.jar:/usr/share/groovy/"
                     "embeddable/groovy-all-1.7.5.jar %s" % (BOT, BOT)),
    "Haskell":      "%s/" + BOT,
    "Java":         "java -jar %%s/%s.jar" % BOT,
    "Javascript":   "node %%s/%s.js" % BOT,
    "Lisp":         "%s/" + BOT,
    "OCaml":        "%%s/%s.native" % BOT,
    "Perl":         "perl %%s/%s.pl" % BOT,
    "PHP":          "php %%s/%s.php" % BOT,
    "Python":       "python %%s/%s.py" % BOT,
    "Ruby":         "ruby %%s/%s.rb" % BOT,
}

def is_valid_language(lang):
    return lang in commands

def get_command(lang, path):
    """ Returns None if lang is not a valid language. """
    c = commands.get(lang)
    if c:
        return c % path
