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
import traceback

from optparse import OptionParser

from server_info import server_info
from submission_hash import hash_file_sha

import compiler
from engine import run_game

# Set up logging
log = logging.getLogger('worker')
log.setLevel(logging.DEBUG)
handler = logging.handlers.RotatingFileHandler("worker.log",
                                               maxBytes=1000000,
                                               backupCount=5)
handler.setLevel(logging.DEBUG)
handler2 = logging.StreamHandler()
formatter = logging.Formatter("%(asctime)s - " + str(os.getpid()) +
                              " - %(levelname)s - %(message)s")
handler.setFormatter(formatter)
handler2.setFormatter(formatter)
log.addHandler(handler)
log.addHandler(handler2)

handler2 = logging.StreamHandler()


STATUS_CREATED = 10
STATUS_UPLOADED = 20
STATUS_COMPILING = 30
# these 4 will be returned by the worker
STATUS_RUNABLE = 40
STATUS_DOWNLOAD_ERROR = 50
STATUS_UNPACK_ERROR = 60
STATUS_COMPILE_ERROR = 70
STATUS_TEST_ERROR = 80

# get game from ants dir
sys.path.append("../ants")
from ants import Ants

class CD(object):
    def __init__(self, new_dir):
        self.new_dir = new_dir

    def __enter__(self):
        self.org_dir = os.getcwd()
        os.chdir(self.new_dir)
        return self.new_dir

    def __exit__(self, type, value, traceback):
        os.chdir(self.org_dir)
          
class GameAPIClient:
    def __init__(self, base_url, api_key):
        self.base_url = base_url
        self.api_key = api_key
    
    def get_url(self, method):
        return '%s/%s.php?api_key=%s' % (self.base_url, method, self.api_key)

    def get_task(self):
        try:
            url = self.get_url('api_get_task')
            data = urllib.urlopen(url).read()
            return json.loads(data)
        except ValueError as ex:
            log.error("Bad json from server during get task: %s" % data)
            return None
        except Exception as ex:
            log.error("Get task error: %s" % ex)
            return None
    
    def get_submission_hash(self, submission_id):
        try:
            url = self.get_url('api_get_submission_hash')
            url += '&submission_id=%s' % submission_id
            data = json.loads(urllib.urlopen(url).read())
            return data['hash']
        except ValueError as ex:
            log.error("Bad json from server during get sumbission hash: %s" % data)
            return None
        except Exception as ex:
            log.error("Get submission hash error: %s" % ex)
            return None
    
    def get_submission(self, submission_id, download_dir):
        try:
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
        except Exception as ex:
            log.error("Get submission error: %s" % ex)
            return None

    def get_map(self, map_filename):
        try:
            url = '%s/maps/%s' % (self.base_url, map_filename)
            log.info("Downloading map %s" % url)
            data = urllib.urlopen(url).read()
            return data.read()
        except Exception as ex:
            log.error("Get map error: %s" % ex)
            return None

    # TODO: save failed posts locally and retry on worker startup
    def post_result(self, method, result):
        # retry 10 times or until post is successful
        retry = 1
        for i in range(retry):
            url = self.get_url(method)
            log.info(url)
            json_data = json.dumps(result)
            hash = hashlib.md5(json_data).hexdigest()
            log.debug("Posting result %s: %s" % (method, json_data))
            log.info("Posting hash: %s" % hash)
            response = urllib.urlopen(url, json.dumps(result))
            if response.getcode() == 200:
                data = response.read()
                try:
                    log.debug(data)
                    data = json.loads(data)["hash"]
                    log.info("Server returned hash: %s" % data)
                    if hash == data:
                        break
                    elif i < retry-1:
                        time.sleep(5)
                except ValueError as ex:
                    log.info("Bad json from server during post result: %s" % data)
                    if i < retry-1:
                        time.sleep(5)                    
            else:
                log.warning("Server did not receive post: %s, %s" % (response.getcode(), response.read()))
                time.sleep(5)

class Worker:
    def __init__(self):
        self.cloud = GameAPIClient( server_info['api_base_url'], server_info['api_key'])
        self.post_id = 0
        self.test_map = None

    def submission_dir(self, submission_id):
        return os.path.join(server_info["submissions_path"], "c" + str(submission_id))
        
    def download_dir(self, submission_id):
        return os.path.join(server_info["submissions_path"], "d" + str(submission_id))

    def download_submission(self, submission_id):
        submission_dir = self.submission_dir(submission_id)
        download_dir = self.download_dir(submission_id)
        if os.path.exists(submission_dir):
            log.info("Already downloaded and compiled: %s..." % submission_id)
            return True
        elif os.path.exists(download_dir):
            log.info("Already downloaded: %s..." % submission_id)
            return True
        else:
            log.info("Downloading %s..." % submission_id)
            os.mkdir(download_dir)
            os.chmod(download_dir, 0755)
            filename = self.cloud.get_submission(submission_id, download_dir)
            if filename != None:
                remote_hash = self.cloud.get_submission_hash(submission_id)
                local_hash = hash_file_sha(filename)
                if local_hash != remote_hash:
                    log.error("After downloading submission %s to %s hash didn't match" %
                            (submission_id, download_dir))
                    log.error("local_hash: %s , remote_hash: %s" % (local_hash, remote_hash))
                    shutil.rmtree(download_dir)
                    log.error("Hash error.")
                    return False
                return True
            else:
                shutil.rmtree(download_dir)
                log.error("Submission not found on server.")
                return False
            
    def clean_old_submission(self, submission_dir):
        with CD(submission_dir):
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
        
    def unpack(self, submission_id):
        download_dir = self.download_dir(submission_id)
        log.info("Unpacking %s..." % download_dir)
        with CD(download_dir):
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
                    log.info("unnzip status: %s" % os.system(command))
                    found_archive_file = True
                    for dirpath, dirnames, filenames in os.walk(".."):
                        os.chmod(dirpath, 755)
                        for filename in filenames:
                            filename = os.path.join(dirpath, filename)
                            os.chmod(filename,stat.S_IMODE(os.stat(filename).st_mode) | stat.S_IRGRP | stat.S_IROTH)
                    break
            if not found_archive_file:
                return False
            self.clean_old_submission(download_dir)
            return True

    def compile(self, submission_id=None, report_status=False):
        def report(status=STATUS_RUNABLE):
            if report_status:
                self.post_id += 1
                result = {"post_id": self.post_id,
                          "submission_id": submission_id, 
                          "status_id": status }
                self.cloud.post_result('api_compile_result', result)
        if submission_id == None:
            # compile in current directory
            compiler.compile_anything()
        else:
            submission_dir = self.submission_dir(submission_id)
            download_dir = self.download_dir(submission_id)
            if os.path.exists(submission_dir):
                log.info("Already compiled: %s" % submission_id)
                if self.functional_test(submission_id):
                    report(STATUS_RUNABLE)
                    return True
                else:
                    report(STATUS_TEST_ERROR)
                    return False
            if not os.path.exists(download_dir):
                if not self.download_submission(submission_id):
                    report(STATUS_DOWNLOAD_ERROR)
                    log.error("Download Error")
                    return False
            if len(os.listdir(download_dir)) == 1:
                if not self.unpack(submission_id):
                    report(STATUS_UNPACK_ERROR)
                    log.error("Unpack Error")
                    return False
            log.info("Compiling %s " % submission_id)
            detected_lang, errors = compiler.compile_anything(download_dir)
            if not detected_lang:
                shutil.rmtree(download_dir)
                log.error('\n'.join(errors))
                report(STATUS_COMPILE_ERROR);
                log.error("Compile Error")
                return False
            else:
                os.rename(download_dir, submission_dir)
                if self.functional_test(submission_id):
                    report(STATUS_RUNABLE)
                    return True
                else:
                    log.error("Functional Test Error")
                    report(STATUS_TEST_ERROR)
                    return False

    def check_hash(self, submission_id):
        try:
            for filename in os.listdir(os.path.join(server_info["submissions_path"], str(submission_id))):
                if filename.endswith(".zip") or filename.endswith(".tgz"):
                    log.info("%s: %s" % (filename, hash_file_sha1(filename)))
        except:
            log.error("Submission path not found.")
    
    def get_map(self, map_filename):
        map_file = os.path.join(server_info["maps_path"], map_filename)
        if not os.path.exists(map_file):
            data = self.cloud.get_map(map_filename)
            if data == None:
                raise Exception("map", "Could not download map from main server.")
            f = open(map_file, 'w')
            f.write(data)
            f.close()
        else:
            f = open(map_file, 'r')
            data = f.read()
            f.close()
        return data
    
    def get_test_map(self):
        if self.test_map == None:
            f = open('../ants/submission_test/test.map', 'r')
            self.test_map = f.read()
            f.close()
        return self.test_map
    
    def functional_test(self, submission_id):
        self.post_id += 1
        try:
            matchup_id = 0
            log.info("Running functional test for %s" % submission_id)
            options = server_info["game_options"]
            options["map"] = self.get_test_map()
            options["output_json"] = True
            game = Ants(options)
            submission_dir = self.submission_dir(submission_id)
            bots = [("../ants/submission_test/", "python TestBot.py"),
                    (submission_dir, compiler.get_run_cmd(submission_dir))]
            result = run_game(game, bots, options, matchup_id)
            log.info(result)
            return True
        except Exception as ex:
            log.error(traceback.format_exc())
            return False
        
    def game(self, task, report_status=False):
        self.post_id += 1
        try:
            matchup_id = int(task["matchup_id"])
            log.info("Running game %s..." % matchup_id)
            if 'options' in task:
                options = task["options"]
            else:
                options = server_info["game_options"]
            options["map"] = self.get_map(task['map_filename'])
            options["output_json"] = True
            game = Ants(options)
            bots = []
            for submission_id in task["players"]:
                if self.compile(submission_id):
                    submission_dir = self.submission_dir(submission_id)
                    run_cmd = compiler.get_run_cmd(submission_dir)
                    #run_dir = tempfile.mkdtemp(dir=server_info["submissions_path"])
                    bots.append((submission_dir, run_cmd))
                    #shutil.copytree(submission_dir, run_dir)
                else:
                    raise Exception('bot', 'Can not compile bot %s' % submission_id)
            output_dir = os.path.join(server_info["root_path"], "games", str(matchup_id))
            if not os.path.exists(output_dir):
                os.mkdir(output_dir)
            options['output_dir'] = output_dir
            options['log_input'] = True
            options['log_output'] = True
            result = run_game(game, bots, options, matchup_id)
            result['matchup_id'] = task['matchup_id']
            result['post_id'] = self.post_id
            if report_status:
                self.cloud.post_result('api_game_result', result)
        except Exception as ex:
            import traceback
            traceback.print_exc()
            result = {"post_id": self.post_id,
                      "matchup_id": matchup_id,
                      "error": str(ex) }
            self.cloud.post_result('api_game_result', result)
            
    def task(self):
        task = self.cloud.get_task()
        if task:
            log.info("Recieved task: %s" % task)
            if task['task'] == 'compile':
                submission_id = int(task['submission_id'])
                self.compile(submission_id, True)
            elif task['task'] == 'game':
                self.game(task, True)
            else:
                time.sleep(20)
        else:
            log.error("Error retrieving task from server.")

def main(argv):
    usage ="""Usage: %prog [options]\nThe worker will not attempt to retrieve
    tasks from the server if a specifec submission_id is given."""
    parser = OptionParser(usage=usage)
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

    # print hash values for submission, must be downloaded
    if opts.submission_id != 0 and opts.hash:
        worker.check_hash(opts.submission_id)
        return

    # download and compile
    if opts.submission_id != 0 and opts.download and opts.compile:
        worker.compile(opts.submission_id)
        return
    
    # download submission
    if opts.submission_id != 0 and opts.download:
        if worker.download_submission(opts.submission_id):
            worker.unpack(opts.submission_id)
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
        if opts.num_tasks <= 0:
            try:
                while True:
                    log.info("Getting task infinity + 1")
                    worker.task()
            except KeyboardInterrupt:
                log.info("[Ctrl] + C, Stopping worker")
        else:
            for task_count in range(opts.num_tasks):
                log.info("Getting task %s" % (task_count + 1))
                worker.task()
        return
    
    parser.print_help()
    
if __name__ == '__main__':
    main(sys.argv[1:])
