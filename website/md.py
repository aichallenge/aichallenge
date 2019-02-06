#!/usr/bin/env python
import sys
import markdown

from mdx_github import MarkdownGithub
from mdx_latex import MarkdownLatex

md = markdown.Markdown(
        extensions=['extra',
                    'codehilite',
                    'toc',
                    'wikilinks',
                    MarkdownLatex(),
                    MarkdownGithub()]
     )
mdfile = open(sys.argv[1], 'r')
mdtext = mdfile.read()
mdfile.close()
sys.stdout.write(md.convert(mdtext))
sys.stdout.write('\n');
