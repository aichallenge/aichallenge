#!/usr/bin/python

import argparse
import time
from datetime import datetime, timedelta

import MySQLdb
from server_info import server_info

DEFAULT_BUFFER = 30
MAX_FILL = 100

def log(msg, *args):
    timestamp = time.asctime()
    msg = msg % args
    print "%s: %s" % (timestamp, msg)

def parse_time(time_str):
    return datetime.strptime(time_str, "%Y-%m-%d %H:%M")

INITIAL_INSERT = """insert into settings (name, number)
    values ("pairing_cutoff", %d)"""
CUTOFF_QUERY = """select number from settings where name = 'pairing_cutoff'"""
UPDATE_QUERY = """update settings set number = %d where name='pairing_cutoff'"""

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("cutoff", type=int,
            help="Target cutoff")
    time_opts = parser.add_mutually_exclusive_group()
    time_opts.add_argument("--minutes", "-m", type=int,
            help="Time to reach target cutoff, in minutes")
    time_opts.add_argument("--time", "-t", type=parse_time,
            help='Time to reach target cutoff, in "Year-Month-Day Hours:Minutes" format')
    parser.add_argument("--commit", "-c", action="store_true", default=False,
            help="Write changes to the database")
    args = parser.parse_args()

    start_time = datetime.now()
    if args.minutes:
        target_time = datetime.now() + timedelta(minutes=args.minutes)
    elif args.time:
        target_time = args.time
    else:
        target_time = datetime.now() + timedelta(hours=8)
    target_cutoff = args.cutoff
    log("Target cutoff is %d at %s" % (target_cutoff, target_time))

    connection = MySQLdb.connect(host = server_info["db_host"],
                                 user = server_info["db_username"],
                                 passwd = server_info["db_password"],
                                 db = server_info["db_name"])
    cursor = connection.cursor()

    cursor.execute(CUTOFF_QUERY)
    if cursor.rowcount > 0:
        initial_cutoff = cursor.fetchone()[0]
        log("Initial cutoff found as %d", initial_cutoff)
    else:
        cursor.execute("select max(rank) + 1 from submission where latest = 1")
        initial_cutoff = cursor.fetchone()[0]
        log("No cutoff found, effective starting cutoff %d", initial_cutoff)
        if args.commit:
            #cursor.execute(INITIAL_INSERT % (initial_cutoff,))
            pass
    current_cutoff = initial_cutoff

    cutoff_diff = target_cutoff - initial_cutoff
    total_time = target_time - start_time
    total_time = total_time.total_seconds()
    log("Changing cutoff by %d in %d minutes", cutoff_diff, total_time / 60)
    while current_cutoff < target_cutoff:
        now = datetime.now()
        to_go = target_time - now
        to_go = to_go.total_seconds()
        log ("%.1f minutes left of %.1f", to_go / 60., total_time / 60.)
        if to_go > 0:
            time_completed = 1 - (to_go / total_time)
            next_cutoff = initial_cutoff + (cutoff_diff * time_completed)
        else:
            time_completed = 1.0
            next_cutoff = target_cutoff
        log("%.0f%% done, changing cutoff from %d to %d",
                time_completed * 100, current_cutoff, next_cutoff)
        if args.commit:
            cursor.execute(UPDATE_QUERY % (next_cutoff,))
        else:
            log("WARNING: Database not updated, -c option not given")
        time.sleep(30)
        cursor.execute(CUTOFF_QUERY)
        if cursor.rowcount > 0:
            current_cutoff = cursor.fetchone()[0]
        else:
            log("WARNING: No cutoff found in database")


if __name__ == "__main__":
    main()

