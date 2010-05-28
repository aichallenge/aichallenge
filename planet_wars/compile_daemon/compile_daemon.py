import os
import sys
import compile_anything

# Looks in the given directory for a file called entry.zip, entry.tar.gz, or
# entry.gz. Unpacks it. If no file was successfully unzipped, returns False.
# Otherwise returns True.
def unpack(path):
  zip_files = [
    ("entry.tar.gz", "tar xfz"),
    ("entry.tgz", "tar xfz"),
    ("entry.zip", "unzip")
  ]
  found_archive_file = False
  for file in zip_files:
    file_name = file[0]
    command = file[1]
    if os.path.exists(file_name):
      print "Found " + file_name
      os.system(command + " " + file_name)
      found_archive_file = True
      break
  return found_archive_file

# Given a submission_id, compiles the submission. If the compile was
# successful, return True. Otherwise, returns False. Also sends mail to the
# user letting them know how the compile went.
def compile_submission(submission_id, email_address):
  compile_output = ""
  path = "~/planet_wars/submissions/" + str(submission_id) + "/"
  if unpack(path):
    os.chdir(path)
    compile_anything()
  else:
    compile_error += "Failed to unpack the submission. This is probably " + \
      "because the submission zip file was missing. This shouldn't " + \
      "happen. Please let the contest administrators know!\n"
  return len(compile_error) = 0
