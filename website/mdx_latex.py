"""
Copyright (c) 2011 Justin Bruce Van Horne

Python-Markdown LaTeX Extension
===============================

Adds support for $math mode$ and %text mode%. This plugin supports
multiline equations/text.

The actual image generation is done via LaTeX/DVI output.
It encodes data as base64 so there is no need for images directly.
All the work is done in the preprocessor.
"""

import re
import os
import string
import base64
import tempfile
import markdown


from subprocess import call, PIPE


# %TEXT% mode which is the default LaTeX mode.
TEX_MODE = re.compile(r'(?=(?<!\\)\\\[).(.+?)(?<!\\)\\\]',
        re.MULTILINE | re.DOTALL)

# $MATH$ mode which is the typical LaTeX math mode.
MATH_MODE = re.compile(r'(?=(?<!\\)\$).(.+?)(?<!\\)\$',
        re.MULTILINE)

# %%PREAMBLE%% text that modifys the LaTeX preamble for the document
PREAMBLE_MODE = re.compile(r'(?=(?<!\\)\$\$\$).(.+?)(?<!\\)\$\$\$',
        re.MULTILINE | re.DOTALL)

# Defines our basic inline image
IMG_EXPR = "<img class='latex-inline math-%s' alt='%s' id='%s'" + \
        " src='data:image/png;base64,%s'>"


# Base CSS template
IMG_CSS = "<style>img.latex-inline { vertical-align: middle; }</style>\n"


class LaTeXPreprocessor(markdown.preprocessors.Preprocessor):
    # These are our cached expressions that are stored in latex.cache
    cached = {}

    # Basic LaTex Setup as well as our list of expressions to parse
    tex_preamble = r"""\documentclass{article}
\usepackage{amsmath}
\usepackage{amsthm}
\usepackage{amssymb}
\usepackage{bm}
\usepackage[usenames,dvipsnames]{color}
\pagestyle{empty}
"""

    def __init__(self, configs):
        try:
            cache_file = open('latex.cache', 'r+')
            for line in cache_file.readlines():
                key, val = line.strip("\n").split(" ")
                self.cached[key] = val
        except IOError:
            pass

    """The TeX preprocessor has to run prior to all the actual processing
    and can not be parsed in block mode very sanely."""
    def _latex_to_base64(self, tex, math_mode):
        """Generates a base64 representation of TeX string"""
        # Generate the temporary file
        tempfile.tempdir = ""
        path = tempfile.mktemp()
        tmp_file = open(path, "w")
        tmp_file.write(self.tex_preamble)

        # Figure out the mode that we're in
        if math_mode:
            tmp_file.write("$%s$" % tex)
        else:
            tmp_file.write("%s" % tex)

        tmp_file.write('\n\end{document}')
        tmp_file.close()

        # compile LaTeX document. A DVI file is created
        status = call(('latex -halt-on-error %s' % path).split(), stdout=PIPE)

        # clean up if the above failed
        if status:
            self._cleanup(path, err=True)
            raise Exception("Couldn't compile LaTeX document." +
                "Please read '%s.log' for more detail." % path)

        # Run dvipng on the generated DVI file. Use tight bounding box.
        # Magnification is set to 1200
        dvi = "%s.dvi" % path
        png = "%s.png" % path

        # Extract the image
        cmd = "dvipng -q -T tight -x 1200 -z 9 \
                %s -o %s" % (dvi, png)
        status = call(cmd.split(), stdout=PIPE)

        # clean up if we couldn't make the above work
        if status:
            self._cleanup(path, err=True)
            raise Exception("Couldn't convert LaTeX to image." +
                    "Please read '%s.log' for more detail." % path)

        # Read the png and encode the data
        png = open(png, "rb")
        data = png.read()
        data = base64.b64encode(data)
        png.close()

        self._cleanup(path)

        return data

    def _cleanup(self, path, err=False):
        # don't clean up the log if there's an error
        extensions = ["", ".aux", ".dvi", ".png", ".log"]
        if err:
            extensions.pop()

        # now do the actual cleanup, passing on non-existent files
        for extension in extensions:
            try:
                os.remove("%s%s" % (path, extension))
            except (IOError, OSError):
                pass

    def run(self, lines):
        """Parses the actual page"""
        # Re-creates the entire page so we can parse in a multine env.
        page = "\n".join(lines)

        # Adds a preamble mode
        preambles = PREAMBLE_MODE.findall(page)
        for preamble in preambles:
            self.tex_preamble += preamble + "\n"
            page = PREAMBLE_MODE.sub("", page, 1)
        self.tex_preamble += "\n\\begin{document}\n"

        # Figure out our text strings and math-mode strings
        tex_expr = [(TEX_MODE, False, x) for x in TEX_MODE.findall(page)]
        tex_expr += [(MATH_MODE, True, x) for x in MATH_MODE.findall(page)]

        # No sense in doing the extra work
        if not len(tex_expr):
            return page.split("\n")

        # Parse the expressions
        new_cache = {}
        for reg, math_mode, expr in tex_expr:
            simp_expr = filter(unicode.isalnum, expr)
            if simp_expr in self.cached:
                data = self.cached[simp_expr]
            else:
                data = self._latex_to_base64(expr, math_mode)
                new_cache[simp_expr] = data
            expr = expr.replace('"', "").replace("'", "")
            page = reg.sub(IMG_EXPR %
                    (str(math_mode).lower(), simp_expr,
                        simp_expr[:15], data), page, 1)

        # Cache our data
        cache_file = open('latex.cache', 'a')
        for key, value in new_cache.items():
            cache_file.write("%s %s\n" % (key, value))
        cache_file.close()

        # Make sure to resplit the lines
        return page.split("\n")


class LaTeXPostprocessor(markdown.postprocessors.Postprocessor):
        """This post processor extension just allows us to further
        refine, if necessary, the document after it has been parsed."""
        def run(self, text):
            # Inline a style for default behavior
            text = IMG_CSS + text
            return text


class MarkdownLatex(markdown.Extension):
    """Wrapper for LaTeXPreprocessor"""
    def extendMarkdown(self, md, md_globals):
        # Our base LaTeX extension
        md.preprocessors.add('latex',
                LaTeXPreprocessor(self), ">html_block")
        # Our cleanup postprocessing extension
        md.postprocessors.add('latex',
                LaTeXPostprocessor(self), ">amp_substitute")


def makeExtension(configs=None):
    """Wrapper for a MarkDown extension"""
    return MarkdownLatex(configs=configs)
