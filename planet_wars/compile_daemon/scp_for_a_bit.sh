#!/bin/bash
for i in {1..5}
do
  python ~/tron/submission_pipeline/scp_new_submissions.py
  sleep 10
done
