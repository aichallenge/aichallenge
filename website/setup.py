#!/usr/bin/env python
import glob
import re
import os

competition="Ants"
seewikiregex=re.compile(r'(<MarkdownReplacement with="([^\n>]*.md)" />)',re.DOTALL)
markdownlocation = "aichallenge.wiki/"

for page in glob.iglob("*.php"):
    pagecontent=open(page).read()
    
    matches=(match.groups() for match in seewikiregex.finditer(pagecontent))
    for toreplace, markdownfilename in matches:
        markdownfilename=markdownfilename.replace("competition",competition)
        print "Inserting `%s` into `%s`, where `%s` was." % (markdownfilename,page,toreplace)
        compiledmarkdown=os.popen("markdown %s" % markdownlocation+markdownfilename).read()
        compiledmarkdown='<!--<MarkdownReplacement from="%s">-->%s<!--</MarkdownReplacement>-->' % (markdownfilename,compiledmarkdown)
        pagecontent=pagecontent.replace(toreplace,compiledmarkdown)
    open(page).write(pagecontent)
    