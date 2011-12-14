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
SSH_COMMAND="ssh -i %s -l ubuntu %%s" % WORKER_KEY

logging.basicConfig(level=logging.INFO, format="~~worker_ssh~~(%(levelname)s): %(message)s")

DEVNULL=open("/dev/null","w")

def get_workers():
    """get the list of workers, last $limit workers"""
    connection = MySQLdb.connect(host = server_info["db_host"],
                                 user = server_info["db_username"],
                                 passwd = server_info["db_password"],
                                 db = server_info["db_name"])
    cursor = connection.cursor()
    cursor.execute(sql["select_workers"])
    return cursor.fetchall()

def ping(worker,count=4):
    """Pings and returns false if 100% packet loss"""
    ip=worker[1]
    
    ping = subprocess.Popen(
        ["ping", "-c", str(count), str(ip)],
        stdout = subprocess.PIPE,
        stderr = subprocess.PIPE
    )
    
    out, error = ping.communicate()
    
    #if anything is wrong stdout will be blank, therefore failure
    if len(out)==0:
        return False
    
    #search for packet loss and return the percentage
    return int(re.search("(\d*)\% packet loss",out).group(1))!=100

def tcpping(worker):
    """Opens a socket to the ssh port, tests if successful. Returns false on timeout(2.0s), connection refused, unknown hostname or no route to host."""
    import socket
    
    try:
        logging.debug("Getting ip of %r" % (worker,))
        ip=socket.gethostbyname(worker[1])
        logging.debug("IP of %r retrieved as %s!" % (worker,ip))
        
        logging.debug("Creating a new socket and connecting to %r..." % (worker,))
        s = socket.create_connection((ip, 22),2.0)
        logging.debug("Connected to %r!" % (worker,))
        
        logging.debug("Closing connection to %r." % (worker,))
        s.close()
    except socket.error as e:
        logging.debug("%r has a socket.error(%s)" % (worker,e))
        return False
    except socket.gaierror as e:
        logging.debug("%r has a socket.gaierror(%s)" % (worker,e))
        return False
    return True

def sshping(worker):
    """Tries to connect via ssh to host, returns true only if publickey was accepted(false if a ssh server was found but asked for password)."""
    host=worker[1]
    command=(SSH_COMMAND + " -oBatchMode=yes -oStrictHostKeyChecking=no -oConnectTimeout=5 -oUserKnownHostsFile=/dev/null exit") % host
    status = subprocess.call(command, shell=True, stdout=DEVNULL, stderr=DEVNULL)
    return status==0

def aliveworkers(workers):
    """returns a list of workers that are alive, packetloss<100"""
    
    #ping everyone using threads
    threads=[]
    results={}
    output=threading.Lock()
    
    def threadcode(worker):
        worker=worker[:]
        logging.info("Pinging %r" % (worker,))
        results[worker]=sshping(worker)
        logging.info("Worker %r is %s." % (worker, ["down","up"][results[worker]]))
    
    
    try:
        for i,worker in enumerate(workers):
            threads.append(threading.Thread())
            threads[i].run=lambda: threadcode(worker)
            threads[i].start()
            threads[i].join(0.1)
        
        #wait for threads to finish
        for thread in threads:
            thread.join()
    except KeyboardInterrupt:
        logging.info("KeyboardInterrupt! Saving alive workers cache.")
        pass
    
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
    subprocess.call(SSH_COMMAND % host, shell=True)

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
        print "Workers that are online(%s):" % len(workers)
        for worker in sorted(workers.items()):
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
                host=(hostid, hostid)
                for workerid,workerhostname in allworkers.items():
                    if hostid==workerhostname:
                        host=(workerid,workerhostname)
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
        print "-a connects to all workers sequentially"
        print "If worker_ids are given it will load the active worker list and connect to them."
        print "If ips are given it will just connect to them."
