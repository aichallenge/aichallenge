#!/usr/bin/python

import os.path
import sys

from install_tools import CD, run_cmd

ARCHIVE_CMD = "git archive --format=tar --prefix=aichallenge/ HEAD | gzip > "

def main(output_directory):
    repo_root = run_cmd("git rev-parse --show-toplevel", True).strip()
    output_directory = os.path.abspath(output_directory)
    with CD(repo_root):
        run_cmd(ARCHIVE_CMD + os.path.join(output_directory, "worker-src.tgz"))

if __name__ == "__main__":
    out_dir = "."
    if len(sys.argv) > 1:
        out_dir = sys.argv[1]
    main(out_dir)

