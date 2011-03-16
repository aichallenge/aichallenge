#!/usr/bin/python

import httplib
import os
import sys
import time
from subprocess import Popen, PIPE

API_KEY = os.getenv("AI_QUERY_KEY")
STORE_FILE = "instance_data"

import json

def get_ec2_instances():
    data = {}
    api_proc = Popen("ec2-describe-instances", shell=True, stdout=PIPE)
    api_out, _ = api_proc.communicate()
    for line in api_out.splitlines():
        fields = line.split('\t')
        if fields[0] != 'INSTANCE' or fields[5] != 'running':
            continue
        data[fields[16]] = fields[1]
    return data

def get_old_data():
    try:
        dfile = open(STORE_FILE)
        data = json.load(dfile)
        dfile.close()
    except (IOError, ValueError):
        data = {}
    return data

def get_worker_game_data(ip):
    con = httplib.HTTPConnection("ai-contest.com")
    con.request("GET", "/api_worker_query.php?api_query_key=%s&ip=%s" % (
        API_KEY, ip))
    result = con.getresponse()
    body = result.read()
    try:
        data = json.loads(body)
    except ValueError:
        print "IP: %s\nresult body:" % (ip,)
        print body
        raise
    if data.has_key('error'):
        data = {'gpm': 0.0, 'epm': 0.0, 'id': None}
    else:
        data['gpm'] = float(data['gpm'])
        data['epm'] = float(data['epm'])
    return data

def reboot_instance(instance_id):
    os.system("ec2-reboot-instances %s" % (instance_id,))

def write_data(data):
    dfile = open(STORE_FILE, 'w')
    json.dump(data, dfile, sort_keys=True, indent=2)
    dfile.close()

old_store = get_old_data()
instances = get_ec2_instances()
new_store = {}
for worker in instances.keys():
    worker_data = get_worker_game_data(worker)
    if old_store.has_key(worker):
        worker_data['boot_time'] = old_store[worker]['boot_time']
    else:
        worker_data['boot_time'] = time.time()
    new_store[worker] = worker_data

min_age = time.time() - (60 * 30)
for worker in new_store.keys():
    worker_data = new_store[worker]
    if ((worker_data['boot_time'] < min_age and worker_data['gpm'] < 4) or
            (worker_data['gpm'] > 8 and
                worker_data['epm'] > worker_data['gpm'] * 0.8)):
        print "%s: Rebooting %s at %s as worker %s with %s gpm %s epm" % (
                time.asctime(), instances[worker], worker, worker_data['id'],
                worker_data['gpm'], worker_data['epm'])
        reboot_instance(instances[worker])
        worker_data['boot_time'] = time.time()

write_data(new_store)
