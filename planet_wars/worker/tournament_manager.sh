#!/bin/sh
sleep 45 # Let the rankings processing happen first -- DVF
cd `dirname $0`
python tournament_manager.py $1
