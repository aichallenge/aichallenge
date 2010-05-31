#!/bin/sh
cd `dirname $0`
./readgames.py | BayesElo/bayeselo
./process_ratings.py ratings.txt ratings_temp.csv
mv ratings_temp.csv ratings.csv
