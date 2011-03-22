#!/bin/sh

URL="http://www.xs4all.nl/~euqirea/projects/google-ai-challenge/ant-wars/3rd-party.tar.bz2";

if [ ! -d 3rd-party ]; then
    echo "This command should be run from the Common Lisp starter bot root directory."
    exit 1;
fi

if command -v curl 1>&2 /dev/null; then
    curl http://www.xs4all.nl/~euqirea/projects/google-ai-challenge/ant-wars/3rd-party.tar.bz2 > 3rd-party.tar.bz2;
elif command -v wget 1>&2 /dev/null; then
    wget curl http://www.xs4all.nl/~euqirea/projects/google-ai-challenge/ant-wars/3rd-party.tar.bz2;
else
    echo "Could not find \"curl\" or \"wget\" commands. Download the libs yourself from:\n$URL";
    exit 1;
fi

if command -v tar 1>&2 /dev/null; then
    tar xvfj 3rd-party.tar.bz2;
else
    echo "3rd-party.tar.bz2 has been downloaded, but \"tar\" command could not be found.";
    exit 1;
fi
