#!/bin/bash
cd `dirname $0`
start_time="$(date +%s)"
python readgames.py | ../../third_party/bayeselo/bayeselo > /dev/null 2> /dev/null
sleep 3
end_time="$(date +%s)"
elapsed_seconds="$(expr $end_time - $start_time)"
python process_ratings.py $elapsed_seconds
