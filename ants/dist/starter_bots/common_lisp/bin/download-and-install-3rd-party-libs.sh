#!/bin/sh

FILE="3rd-party.tar.bz2";
URL="http://www.xs4all.nl/~euqirea/projects/google-ai-challenge/ant-wars/$FILE";

if [ ! -d 3rd-party ]; then
    echo "This command should be run from the Common Lisp starter bot root directory."
    exit 1;
fi

if command -v curl 1>&2 /dev/null; then
    curl $URL > $FILE;
elif command -v wget 1>&2 /dev/null; then
    wget $URL;
else
    echo "Could not find \"curl\" or \"wget\" commands. Download the libs yourself from:\n$URL";
    exit 1;
fi

if command -v tar 1>&2 /dev/null; then
    tar xvfj $FILE;
    rm -f $FILE;
else
    echo "$FILE has been downloaded, but \"tar\" command could not be found.";
    exit 1;
fi
