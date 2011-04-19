#!/usr/bin/env python

import re
import sys
import os
import webbrowser
import json

def generate(data, generated_path):
    path = os.path.dirname(__file__)
    template_path = os.path.join(path, 'replay.html.template')
    template = open(template_path, 'r')
    content = template.read()
    template.close()

    quote_re = re.compile("'")
    newline_re = re.compile("\s", re.MULTILINE)
    insert_re = re.compile(r"## REPLAY PLACEHOLDER ##")
    data = quote_re.sub(r"\\\\'", data)
    data = newline_re.sub("", data)
    content = insert_re.sub(data, content)

    output = open(generated_path, 'w')
    output.write(content)
    output.close()

if __name__ == "__main__":
    data = sys.stdin.read()

    path = os.path.dirname(__file__)
    generated_path = os.path.realpath(os.path.join(path, 'replay.html'))

    generate(data, generated_path)

    # allow people to surpress launching the brower
    launch = True
    if len(sys.argv) > 1 and sys.argv[1] == '--nolaunch':
        launch = False

    # open the page in the browser
    if launch:
        webbrowser.open('file://'+generated_path)
