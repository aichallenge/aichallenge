import os
import sys
max_num_processes = 0
for i in range(50):
  try:
    child_pid = os.fork()
  except:
    break
  if child_pid == 0:
#    max_num_processes = i + 1
  else:
    os.wait()
    sys.exit()
print "processes:", max_num_processes
#import os
#while True:
#  os.fork()
