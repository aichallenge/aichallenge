#!/usr/bin/env python

import re
import sys
import os
import webbrowser
import json

def generate(data, generated_path):
    path = os.path.dirname(__file__)
    template_path = os.path.join(path, 'visualizer', 'replay.html.template')
    template = open(template_path, 'r')
    content = template.read()
    template.close()

    json_data = json.dumps({
        'challenge': 'ants',
        'replayformat': 'storage',
        'replaydata': data
    })
    insert_re = re.compile(r"## REPLAY PLACEHOLDER ##")
    content = insert_re.sub(json_data, content)

    output = open(generated_path, 'w')
    output.write(content)
    output.close()

if __name__ == "__main__":
    data = sys.stdin.read()

    path = os.path.dirname(__file__)
    generated_path = os.path.realpath(os.path.join(path, 'visualizer', 'replay.html'))

    generate(data, generated_path)
    webbrowser.open('file://'+generated_path)
