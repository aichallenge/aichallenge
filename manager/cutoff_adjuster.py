#!/usr/bin/python

import logging
import logging.handlers
import math
import os
import tempfile
import time
from datetime import datetime

START_TIME = datetime(2010, 11, 30, 12, 0)
START_RANK = 3020

END_TIME = datetime(2010, 12, 1, 0, 0)
END_RANK = 1000

CUTOFF_FILE = "/home/contest/pairing_cutoff"

TIME_FMT = "%Y-%m-%d %H:%M"

logger = logging.getLogger('cutoff_logger')
logger.setLevel(logging.INFO)
_my_pid = os.getpid()
def log_message(message):
  logger.info(str(_my_pid) + ": " + message)
  print message

def update_cutoff():
    now = datetime.now()
    if now < START_TIME:
        new_rank = START_RANK
    elif now >= END_TIME:
        new_rank = END_RANK
    else:
        trans_length = END_TIME - START_TIME
        trans_length = (trans_length.days * 3600 * 24) + trans_length.seconds
        current = now - START_TIME
        current = float((current.days * 3600 * 24) + current.seconds)
        rank_diff = END_RANK - START_RANK
        new_rank = START_RANK
        new_rank += int(math.ceil(rank_diff * (current / trans_length)))
    cut_fd, tmp_name = tempfile.mkstemp(dir=".")
    cut_file = os.fdopen(cut_fd, 'w')
    cut_file.write("%d" % (new_rank,))
    cut_file.close()
    os.chmod(tmp_name, 0644)
    os.rename(tmp_name, CUTOFF_FILE)
    log_message("%s: set cutoff to %d"
            % (now.strftime(TIME_FMT), new_rank))

def main():
    try:
        handler = logging.handlers.RotatingFileHandler("cutoff.log",
                                               maxBytes=1000000,
                                               backupCount=5)
        logger.addHandler(handler)
    except IOError:
       # couldn't start the file logger
       pass

    if START_TIME >= END_TIME:
        raise ValueError("Start time must be before end time")
    if END_RANK >= START_RANK:
        raise ValueError("End rank must be less than start rank")
    log_message("Starting cutoff adjust from %d at %s to %d at %s"
            % (START_RANK, START_TIME.strftime(TIME_FMT),
                END_RANK, END_TIME.strftime(TIME_FMT)))
    stop = False
    while not stop:
        if datetime.now() > END_TIME:
            stop = True
        update_cutoff()
        time.sleep(60)

if __name__ == "__main__":
    main()
