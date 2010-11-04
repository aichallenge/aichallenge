#!/usr/bin/python

import os
import sys
from hashlib import sha1
from subprocess import Popen, PIPE

SUB_PATH = "/home/contest/ai-contest/planet_wars/submissions"

EXT_EXCLUDES = frozenset(['.zip', '.tgz'])
NAME_EXCLUDES = frozenset(['PlayGame.jar', 'ShowGame.jar', '.DS_Store', 'Icon'])

def collect_filenames(path):
    filenames = []
    for root, dirs, files in os.walk(path):
        for name in files:
            if name in NAME_EXCLUDES:
                continue
            ext = name.rfind('.')
            if name[ext:] in EXT_EXCLUDES:
                continue
            filenames.append(os.path.join(root, name))
    return filenames

# suprisingly in testing this is slower than doing the file hash in python
def hash_file_md5sum(filename):
    proc = Popen(["md5sum", filename], stdout=PIPE)
    fhash, _ = proc.communicate()
    if proc.returncode != 0:
        raise OSError("md5sum had an error while hashing %s" % (filename,))
    return fhash

def hash_file_sha(filename):
    READ_SIZE = 4096 * 2500
    fhash = sha1()
    fhash.update(filename)
    f = open(filename, 'r')
    content = f.read(READ_SIZE)
    while len(content) != 0:
        fhash.update(content)
        content = f.read(READ_SIZE)
    return fhash.digest()

# this was only around 20-25% faster than the full hash
def hash_file_size(filename):
    fhash = sha1()
    fhash.update(filename)
    fhash.update(str(os.path.getsize(filename)))
    return fhash.digest()

def hash_file(filename):
    return hash_file_sha(filename)

def hash_submission(submission_id):
    sub_dir = os.path.join(SUB_PATH, submission_id)
    sub_files = collect_filenames(sub_dir)
    sub_hash = sha1()
    for name in sub_files:
        sub_hash.update(hash_file(name))
    return sub_hash.hexdigest()

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print "usage: submission_hash.py <submission_id>"
        sys.exit(1)
    submission_id = sys.argv[1]
    print hash_submission(submission_id)

