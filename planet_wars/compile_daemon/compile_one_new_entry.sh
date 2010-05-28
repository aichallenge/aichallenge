#!/bin/bash
echo "fetching a new submission"
php ~/tron/submission_pipeline/submissions_to_be_compiled.php > ~/tron/submission_pipeline/entries_to_be_compiled.txt
exec < ~/tron/submission_pipeline/entries_to_be_compiled.txt
if read line
then
  echo "attempting to compile submission " $line
  php ~/tron/submission_pipeline/update_submission_status.php $line 24
  mkdir ~/tron/compile/$line
  echo "" > ~/tron/compile/$line/submission_error.txt
  cp ~/tron/entries/$line/* ~/tron/compile/$line/
  cd ~/tron/compile/$line
  python ~/tron/submission_pipeline/unpack.py >> compile_output.txt 2>> compile_error.txt
  cp ~/tron/submission_pipeline/run.sh ~/tron/compile/$line/
  python ~/tron/submission_pipeline/compile_anything.py $line >> compile_output.txt 2>> compile_error.txt
  echo "Finished running auto-compile script. Detecting errors, if any."
  echo "Finished running auto-compile script. Detecting errors, if any." >> submission_error.txt
  file_size=`stat -c%s compile_error.txt`
  if [ $file_size -gt 0 ]
  then
    echo "Compiler errors detected. Compilation failed."
    echo "Compiler errors detected. Compilation failed." >> submission_error.txt
    php ~/tron/submission_pipeline/update_submission_status.php $line 70
    php ~/tron/submission_pipeline/send_compile_fail_mail.php $line
    echo "Fail-mail sent."
    exit
  else
    echo "No compile errors detected. Compilation succeeded."
    echo "No compile errors detected. Compilation succeeded." >> submission_error.txt
    php ~/tron/submission_pipeline/update_submission_status.php $line 40
    php ~/tron/submission_pipeline/send_compile_success_mail.php $line
  fi
else
  echo "no new submissions"
fi

