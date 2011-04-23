#!/usr/bin/env python
import os

prefix, competition, suffix = "aichallenge.wiki/", "Ants", ".md"
mapping = {
    "index.php": "%s",
}
toreplace = "ContentFromTheWiki"

for page in mapping.keys():
    mapping[page] = (prefix+mapping[page]+suffix) % (competition,)

if __name__=="__main__":
    for page in mapping.keys():
        pagecontent=open(page).read()
        compiledmarkdown=os.popen("markdown %s" % (mapping[page])).read()
        pagecontent=pagecontent.replace(toreplace,compiledmarkdown)
        print pagecontent
        
