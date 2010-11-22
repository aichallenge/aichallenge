#!/bin/bash
for i in `seq -w 1 $2`;
do
  python map_generator.py > "../maps/map_$1_$i.txt"
done
