#!/usr/bin/env python

import argparse
import worker.process

def worker_command(args):
	if args.command == 'start':
		worker.process.spawn_multiple(args.num_procs)
	elif args.command == 'stop':
		worker.process.stop()

parser = argparse.ArgumentParser()
subparsers = parser.add_subparsers()

worker_parser = subparsers.add_parser('worker')
worker_parser.add_argument('command', choices=['start','stop'])
worker_parser.add_argument('-n', '--num-procs', type=int, default=1)
worker_parser.set_defaults(func=worker_command)

args = parser.parse_args()
args.func(args)
