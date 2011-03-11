import sys
import os
import json
import urllib
import tempfile
import logging
import logging.handlers
import shutil
import hashlib
import time
import stat
import platform

from optparse import OptionParser

from server_info import server_info
from submission_hash import hash_file_sha

import compiler
from engine import run_game

# Set up logging
logger = logging.getLogger('tm_logger')
logger.setLevel(logging.INFO)
handler = logging.handlers.RotatingFileHandler("tm.log",
                                               maxBytes=1000000,
                                               backupCount=5)
logger.addHandler(handler)
tm_pid = os.getpid()

STATUS_CREATED = 10
STATUS_UPLOADED = 20
STATUS_COMPILING = 24
# these 4 will be returned by the worker
STATUS_RUNABLE = 40
STATUS_UNPACK_ERROR = 50
STATUS_COMPILE_ERROR = 70
STATUS_TEST_ERROR = 80

# get game from ants dir
sys.path.append("../ants")
from ants import Ants

def log_message(message):
    logger.info(str(tm_pid) + ": %s" % message)
    print message
  
class GameAPIClient:
    def __init__(self, base_url, api_key):
        self.base_url = base_url
        self.api_key = api_key
    
    def get_url(self, method):
        return '%s/%s.php?api_key=%s' % (self.base_url, method, self.api_key)

    def get_languages(self):
        url = self.get_url('api_get_languages')
        data = urllib.urlopen(url).read()
        return json.loads(data)

    def get_task(self):
        url = self.get_url('api_get_task')
        data = urllib.urlopen(url).read()
        return json.loads(data)        
    
    def get_submission_hash(self, submission_id):
        url = self.get_url('api_get_submission_hash')
        url += '&submission_id=%s' % submission_id
        data = json.loads(urllib.urlopen(url).read())
        return data['hash']
    
    def get_submission(self, submission_id, download_dir):
        url = self.get_url('api_get_submission')
        url += '&submission_id=%s' % submission_id
        remote_zip = urllib.urlopen(url)
        filename = remote_zip.info().getheader('Content-disposition').split('filename=')[1]
        filename = os.path.join(download_dir, filename)
        local_zip = open(filename, 'wb')
        local_zip.write(remote_zip.read())
        local_zip.close()
        remote_zip.close()
        return filename

    def post_result(self, method, result):
        print("Posting result %s: %s" % (method, result))
        # retry 10 times or until post is successful
        for i in range(10):
            url = self.get_url(method)
            json_data = json.dumps(result)
            hash = hashlib.md5(json_data).hexdigest()
            print("Posting hash: %s" % hash)
            data = urllib.urlopen(url, json.dumps(result)).read()
            data = json.loads(data)["hash"]
            print("Server returned hash: %s" % data)
            if hash == data:
                break
            else:
                time.sleep(5)

class Worker:
    def __init__(self):
        self.task_mode = False # if true, report back to server
        self.cloud = GameAPIClient( server_info['api_base_url'], server_info['api_key'])
        self.submissions = {} # memory cache of downloaded submissions and languages
        self.languages = None # memory cache of languages data from database
        self.post_id = 0
    
    def get_languages(self):
        if self.languages == None:
            print("Retrieving languages")
            self.languages = self.cloud.get_languages()
        return self.languages
    
    def check_submissions(self):
        for submission_dir in os.listdir(server_info["submissions_path"]):
            pass

    def download_submission(self, submission_id):
        submission_dir = os.path.join(server_info["submissions_path"], "c" + str(submission_id))
        download_dir = os.path.join(server_info["submissions_path"], "d" + str(submission_id))
        if os.path.exists(submission_dir):
            print("Already downloaded and compiled: %s..." % submission_id)
            return submission_dir
        elif os.path.exists(download_dir):
            print("Already downloaded: %s..." % submission_id)
            if not self.unpack(download_dir):
                return False
            return download_dir
        else:
            print("Downloading %s..." % submission_id)
            os.mkdir(download_dir)
            os.chmod(download_dir, 0755)
            filename = self.cloud.get_submission(submission_id, download_dir)
            remote_hash = self.cloud.get_submission_hash(submission_id)
            local_hash = hash_file_sha(filename)
            if local_hash != remote_hash:
                log_message("After downloading submission %s to %s hash didn't match" %
                        (submission_id, download_dir))
                log_message("local_hash: %s , remote_hash: %s" % (local_hash, remote_hash))
                shutil.rmtree(download_dir)
                return None
            if not self.unpack(download_dir):
                return False
            return download_dir

    def clean_old_submission(self, submission_dir):
        save_dir = os.getcwd()
        os.chdir(submission_dir)
        # this is for dealing with starter packs being submitted
        #    start pack structer should be changed
        files = os.listdir(os.getcwd())
        num_files = 0
        num_dirs = 0
        dir_names = []
        for file in files:
            if os.path.isfile(file):
                num_files += 1
            if os.path.isdir(file):
                dir_names.append(file)
        if len(dir_names) > 0 and num_files == 1:
            for dir_name in dir_names:
                os.system("mv " + str(dir_name) + "/* .")
                os.system("rm -rf " + str(dir_name))
        os.system("rm -rf tools maps example_bots")
        os.chdir(save_dir)
        
    def unpack(self, submission_dir):
        print("Unpacking %s..." % submission_dir)
        save_dir = os.getcwd()
        os.chdir(submission_dir)
        if platform.system() == 'Windows':
            zip_files = [
                ("entry.tar.gz", "7z x -y entry.tar.gz > NUL"),
                ("entry.tgz", "7z x -y entry.tgz > NUL"),
                ("entry.zip", "7z x -y entry.zip > NUL")
            ]
        else:
            zip_files = [
                ("entry.tar.gz", "tar xfz entry.tar.gz > /dev/null 2> /dev/null"),
                ("entry.tgz", "tar xfz entry.tgz > /dev/null 2> /dev/null"),
                ("entry.zip", "unzip -u entry.zip > /dev/null 2> /dev/null")
            ]
        found_archive_file = False
        for file_name, command in zip_files:
            if os.path.exists(file_name):
                print("unnzip status: %s" % os.system(command))
                found_archive_file = True
                for dirpath, dirnames, filenames in os.walk(".."):
                    os.chmod(dirpath, 755)
                    for filename in filenames:
                        filename = os.path.join(dirpath, filename)
                        os.chmod(filename,stat.S_IMODE(os.stat(filename).st_mode) | stat.S_IRGRP | stat.S_IROTH)
                break
        if not found_archive_file:
            return False
        self.clean_old_submission(submission_dir)
        os.chdir(save_dir)
        return True
        
    def compile(self, submission_id=None):
        def report(status=STATUS_RUNABLE):
            if self.task_mode:
                self.post_id += 1
                result = {"post_id": self.post_id,
                          "submission_id": submission_id, 
                          "status_id": status }
                self.cloud.post_result('api_compile_result', result)
        if submission_id == None:
            compiler.compile_anything(self.get_languages())
        else:
            submission_dir = os.path.join(server_info["submissions_path"], "c" + str(submission_id))
            download_dir = os.path.join(server_info["submissions_path"], "d" + str(submission_id))
            if os.path.exists(submission_dir):
                print("Already compiled: %s" % submission_id)
                report(STATUS_RUNABLE)
                return submission_dir
            if not os.path.exists(download_dir):
                if not self.download_submission(submission_id):
                    report(STATUS_UNPACK_ERROR)
                    return None
            log_message("Compiling %s " % submission_id)
            main_dir = os.getcwd()
            os.chdir(download_dir)
            compile_log = compiler.Log()
            success = compiler.compile_anything(self.get_languages())
            os.chdir(main_dir)
            if not success:
                shutil.rmtree(download_dir)
                log_message(compile_log.err)
                log_message("ERROR: could not compile submission")
                report(STATUS_COMPILE_ERROR);
                return None
            else:
                os.rename(download_dir, submission_dir)
                report(STATUS_RUNABLE);
                return submission_dir

    def check_hash(self, submission_id):
        try:
            for filename in os.listdir(os.path.join(server_info["submissions_path"], str(submission_id))):
                if filename.endswith(".zip") or filename.endswith(".tgz"):
                    log_message("%s: %s" % (filename, hash_file_sha1(filename)))
        except:
            log_message("Submission path not found.")
                
    def game(self, task):
        game_id = options(task["game_id"])        
        print("Running game %s..." % game_id)
        options = task["options"]
        options["map"] = urllib.urlopen(options["map_url"]).read()
        options["output_json"] = True 
        game = Ants(options)
        bots = [('.', arg) for arg in args]
        result = run_game(game, bots, options, game_id)
        if self.task_mode:
            self.cloud.post_result('api_game_result', result)
    
    def task(self):
        task = self.cloud.get_task()
        print task
        if task['task'] == 'compile':
            submission_id = int(task['submission_id'])
            if self.download_submission(submission_id):
                self.compile(submission_id)
        elif task['task'] == 'game':
            game(task)

def main(argv):
    usage ="""Usage: %prog [options]\nThe worker will not attempt to retrieve
    tasks from the server if a specifec submission_id is given."""
    parser = OptionParser(usage=usage)
    parser.add_option("-l", "--languages", dest="languages",
                      action="store_true", default=False,
                      help="Get language list from server")
    parser.add_option("-s", "--submission_id", dest="submission_id",
                      type="int", default=0,
                      help="Submission id to use for hash, download and compile")
    parser.add_option("--hash", dest="hash",
                      action="store_true", default=False,
                      help="Display submission hash")
    parser.add_option("-d", "--download", dest="download",
                      action="store_true", default=False,
                      help="Download submission")
    parser.add_option("-c", "--compile", dest="compile",
                      action="store_true", default=False,
                      help="Compile current directory or submission")
    parser.add_option("-t", "--task", dest="task",
                      action="store_true", default=False,
                      help="Get next task from server")
    parser.add_option("-n", "--num_tasks", dest="num_tasks",
                      type="int", default=1,
                      help="Number of tasks to get from server")
    
    (opts, args) = parser.parse_args(argv)
    
    worker = Worker()

    # print languages, use to test worker api key
    if opts.languages:
        print worker.get_languages()
        return

    # print hash values for submission, must be downloaded
    if opts.submission_id != 0 and opts.hash:
        worker.check_hash(opts.submission_id)
        return

    # download and compile
    if opts.submission_id != 0 and opts.download and opts.compile:
        if worker.download_submission(opts.submission_id):
            worker.compile(opts.submission_id)
        else:
            print("Download failed, aborting compile")
        return
    
    # download submission
    if opts.submission_id != 0 and opts.download:
        worker.download_submission(opts.submission_id)
        return

    # compile submission
    if opts.submission_id != 0 and opts.compile:
        worker.compile(opts.submission_id)
        return
    
    # compile in current directory
    if opts.compile:
        worker.compile()
        return   
    
    # get tasks
    if opts.task:
        worker.task_mode = True;
        if opts.num_tasks <= 0:
            try:
                while True:
                    print("Getting task infinity + 1")
                    worker.task()
            except KeyboardInterrupt:
                print("[Ctrl] + C, Stopping worker")
        else:
            for task_count in range(opts.num_tasks):
                print("Getting task %s" % (task_count + 1))
                worker.task()
        return
    
    parser.print_help()
    
if __name__ == '__main__':
    main(sys.argv[1:])
