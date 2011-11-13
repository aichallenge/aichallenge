#!/usr/bin/python

import os
import sys
import subprocess
import re
import threading
import logging

import MySQLdb
from server_info import server_info
from sql import sql

ALIVE_WORKERS_CACHE="/tmp/aliveworkers"
WORKER_KEY="~/workerkey"

logging.basicConfig(level=logging.INFO, format="~~worker_ssh~~(%(levelname)s): %(message)s")

def get_workers(limit=20):
    """get the list of workers, last $limit workers"""
    connection = MySQLdb.connect(host = server_info["db_host"],
                                 user = server_info["db_username"],
                                 passwd = server_info["db_password"],
                                 db = server_info["db_name"])
    cursor = connection.cursor()
    cursor.execute(sql["select_workers"], (limit,))
    return cursor.fetchall()

def ping(worker,count=4):
    """returns packet loss percentage for a worker, 100 means offline"""
    ip=worker[1]
    
    ping = subprocess.Popen(
        ["ping", "-c", str(count), str(ip)],
        stdout = subprocess.PIPE,
        stderr = subprocess.PIPE
    )
    
    out, error = ping.communicate()
    
    #if anything is wrong stdout will be blank, therefore failure
    if len(out)==0:
        return 100
    
    #search for packet loss and return the percentage
    return int(re.search("(\d*)\% packet loss",out).group(1))

def aliveworkers(workers):
    """returns a list of workers that are alive, packetloss<100"""
    
    #ping everyone using threads
    threads=[]
    results={}
    output=threading.Lock()
    
    def threadcode(worker):
        worker=worker[:]
        logging.info("Pinging %r" % (worker,))
        results[worker]=ping(worker)!=100
        logging.info ("Worker %r is %s." % (worker, ["down","up"][results[worker]]))
    
    for i,worker in enumerate(workers):
        threads.append(threading.Thread())
        threads[i].run=lambda: threadcode(worker)
        threads[i].start()
        threads[i].join(0.1)
    
    #wait for threads to finish
    for thread in threads:
        thread.join()
    
    aliveworkers=[worker for worker,result in results.items() if result==True]
    return aliveworkers

def loadaliveworkers(filename=ALIVE_WORKERS_CACHE):
    try:
        return dict(eval(open(filename).read()))
    except :
        logging.warning("%s not found, assuming blank list. Try reloading the alive worker list." % (filename,))
        return {}

def ssh(host):
    logging.info("Connecting to %s:" % (host,))
    host=host[1]
    subprocess.call("ssh -i %s -l ubuntu %s" % (WORKER_KEY, host), shell=True)

if __name__ == "__main__":
    import getopt
    
    optlist, hostids = getopt.getopt(sys.argv[1:], '::ral')
    optlist=dict(optlist)
    
    if "-r" in optlist.keys():
        #regenerate alive worker list
        logging.info("Regenerating alive worker list.")
        workers=get_workers()
        logging.info("Worker list from the database: %s" % (workers,))
        workers=aliveworkers(workers)
        open(ALIVE_WORKERS_CACHE,"w").write(repr(workers))
    
    if "-l" in optlist.keys():
        #list
        workers=loadaliveworkers()
        print "Workers that are online:"
        for worker in workers.items():
            print "\t%d - %s" % (worker)
        if len(workers)==0:
            print "\tAll workers are offline."
    
    if "-a" in optlist.keys():
        #put all hosts in hostsids
        hostids=loadaliveworkers().keys()
        
    if len(hostids)>0 or "-a" in optlist.keys():
        #ssh in all hostids
        allworkers=loadaliveworkers()
        
        hosts=[]
        for hostid in hostids:
            try:
                host=(int(hostid),allworkers[int(hostid)])
            except KeyError as e:
                raise Exception("Worker %s not found. Try reloading the alive worker list. Alive workers: %r" % (e.args[0], allworkers))
            except ValueError:
                #interpret hostid as an ip
                host=("unknownid", hostid)
            hosts.append(host)
        
        logging.info("Will connect to %s" % hosts)
        
        for host in hosts:
            ssh(host)
    
    if "-h" in optlist.keys() or (len(optlist)==0 and len(hostids)==0):
        #show help
        print "worker_ssh.py [-r] [-l] [-a] [worker_id list] [ip list]"
        print
        print "-r to generate a new alive worker list in %s" % ALIVE_WORKERS_CACHE
        print "-l prints the alive worker list"
        print "-a connects to all workers sequencially"
        print "If worker_ids are given it will load the active worker list and connect to them."
        print "If ips are given it will just connect to them."