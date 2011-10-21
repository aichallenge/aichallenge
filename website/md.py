#!/usr/bin/env python
import sys
import markdown
md = markdown.Markdown(
        extensions=['extra',
                    'codehilite',
                    'toc',
                    'wikilinks',
                    'latex',
                    'github']
     )
mdfile = open(sys.argv[1], 'r')
mdtext = mdfile.read()
mdfile.close()
sys.stdout.write(md.convert(mdtext))
sys.stdout.write('\n');
