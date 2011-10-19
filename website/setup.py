#!/usr/bin/env python
import glob
import re
import os



def replaceMarkdown():
    competition="Ants"
    seewikiregex=re.compile(r'(<!--<MarkdownReplacement with="([^\n>]*.md)">-->.*?<!--</MarkdownReplacement>-->)',re.DOTALL)
    markdownlocation = "aichallenge.wiki/"
    
    for page in glob.iglob("*.php"):
        try:
            pagecontent=open(page,"r").read()
            
            matches=(match.groups() for match in seewikiregex.finditer(pagecontent))
            for toreplace, markdownfilename in matches:
                realmarkdownfilename=markdownfilename.replace("competition",competition)
                print "Inserting `%s` into `%s`, where `%s...` was." % (realmarkdownfilename,page,toreplace[:90])
                compiledmarkdown=os.popen("python md.py %s" % markdownlocation+realmarkdownfilename).read()
                compiledmarkdown='<!--<MarkdownReplacement with="%s">-->%s<!--</MarkdownReplacement>-->' % (markdownfilename,compiledmarkdown)
                pagecontent=pagecontent.replace(toreplace,compiledmarkdown)
            open(page,"w").write(pagecontent)
        except IOError:
            print "Ignoring `%s` because of errors" % (page)
            
def concatCSS():
    css_order = ['font.css', 'reset.css', 'layout.css'] # these files go first
    with open('aichallenge.css', 'w') as f:
        css_files = css_order + list(set(os.listdir('css')) - set(css_order))
        for filename in css_files:
            print "Concatenating `%s` to the main css." % (filename)
            if filename.endswith('.css'):
                with open(os.path.join('css', filename), 'r') as fin:
                    f.write('\n/* begin %s %s */\n' % (filename, '='*(70-len(filename))))
                    f.write(fin.read())
                    f.write('\n/* end %s %s */\n' % (filename, '='*(72-len(filename))))



def setup():
    concatCSS()
    replaceMarkdown()

if __name__=="__main__":
    setup()