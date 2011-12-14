#!/usr/bin/env python
from __future__ import print_function
import sys
import os
import json
import urllib
import logging.handlers
import logging
import pickle
import shutil
from hashlib import md5
import time
import stat
import platform
import unicodedata
import traceback
import tempfile
from copy import copy, deepcopy

from optparse import OptionParser

from server_info import server_info

import compiler
from engine import run_game

# Set up logging
log = logging.getLogger('worker')
log.setLevel(logging.INFO)
log_file = os.path.join(server_info.get('logs_path', '.'), 'worker.log')
handler = logging.handlers.RotatingFileHandler(log_file,
                                               maxBytes=10000000,
                                               backupCount=5)
handler.setLevel(logging.INFO)
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
sys.path.append(os.path.join(server_info.get('repo_path', '..'), 'ants'))
from ants import Ants


def uni_to_ascii(ustr):
    return unicodedata.normalize('NFKD', ustr).encode('ascii','ignore')

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
            log.debug(url)
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
            log.debug(url)
            remote_zip = urllib.urlopen(url)
            filename = remote_zip.info().getheader('Content-disposition')
            if filename == None:
                log.error("File not returned by server: {0}".format(remote_zip.read()))
                return None
            filename = filename.split('filename=')[1]
            filename = os.path.join(download_dir, filename)
            local_zip = open(filename, 'wb')
            local_zip.write(remote_zip.read())
            local_zip.close()
            remote_zip.close()
            return filename
        except Exception as ex:
            log.error(traceback.format_exc())
            log.error("Get submission error: %s" % ex)
            return None

    def get_map(self, map_filename):
        try:
            url = '%s/map/%s' % (self.base_url, map_filename)
            log.info("Downloading map %s" % url)
            data = urllib.urlopen(url).read()
            log.debug(data)
            return data
        except Exception as ex:
            log.error("Get map error: %s" % ex)
            return None

    def post_result(self, method, result):
        # save result in case of failure
        with open('last_post.json', 'w') as f:
            try:
                f.write(json.dumps([method, result]))
            except:
                with open('bad_result', 'w') as br:
                    pickle.dump([method, result], br)
                raise
        # retry 100 times or until post is successful
        retry = 100
        wait_time = 2
        for i in range(retry):
            wait_time = min(wait_time * 2, 300)
            try:
                url = self.get_url(method)
                log.info(url)
                if i == 0:
                    json_log = deepcopy(result)
                    if 'replaydata' in json_log:
                        del json_log['replaydata']
                    json_log = json.dumps(json_log)
                    log.debug("Posting result %s: %s" % (method, json_log))
                else:
                    log.warning("Posting attempt %s" % (i+1))
                json_data = json.dumps(result)
                hash = md5(json_data).hexdigest()
                if i == 0:
                    log.info("Posting hash: %s" % hash)
                response = urllib.urlopen(url, json.dumps(result))
                if response.getcode() == 200:
                    data = response.read()
                    try:
                        log.debug(data.strip())
                        data = json.loads(data)["hash"]
                        log.info("Server returned hash: %s" % data)
                        if hash == data:
                            os.remove('last_post.json')
                            break
                        elif i < retry-1:
                            log.warning('Waiting %s seconds...' % wait_time)
                            time.sleep(wait_time)
                    except ValueError:
                        log.warning("Bad json from server during post result: %s" % data)
                        if i < retry-1:
                            log.warning('Waiting %s seconds...' % wait_time)
                            time.sleep(wait_time)
                else:
                    log.warning("Server did not receive post: %s, %s" % (response.getcode(), response.read()))
                    time.sleep(wait_time)
            except IOError as e:
                log.error(traceback.format_exc())
                log.warning('Waiting %s seconds...' % wait_time)
                time.sleep(wait_time)
        else:
            return False
        return True

class Worker:
    def __init__(self, debug=False):
        self.cloud = GameAPIClient( server_info['api_base_url'], server_info['api_key'])
        self.post_id = 0
        self.test_map = None
        self.download_dirs = {}
        self.debug = debug

    def submission_dir(self, submission_id):
        return os.path.join(server_info["compiled_path"], str(submission_id//1000), str(submission_id))

    def download_dir(self, submission_id):
        if submission_id not in self.download_dirs:
            tmp_dir = tempfile.mkdtemp(dir=server_info["compiled_path"])
            self.download_dirs[submission_id] = tmp_dir
        return self.download_dirs[submission_id]

    def clean_download(self, submission_id):
        if not self.debug and submission_id in self.download_dirs:
            d_dir = self.download_dirs[submission_id]
            log.debug('Cleaning up {0}'.format(d_dir))
            if os.path.exists(d_dir):
                shutil.rmtree(d_dir)
            del self.download_dirs[submission_id]

    def download_submission(self, submission_id):
        submission_dir = self.submission_dir(submission_id)
        if os.path.exists(submission_dir):
            log.info("Already downloaded and compiled: %s..." % submission_id)
            return True
        elif submission_id in self.download_dirs:
            log.info("Already downloaded: %s..." % submission_id)
            return True
        else:
            download_dir = self.download_dir(submission_id)
            log.info("Downloading %s..." % submission_id)
            os.chmod(download_dir, 0755)
            filename = self.cloud.get_submission(submission_id, download_dir)
            if filename != None:
                remote_hash = self.cloud.get_submission_hash(submission_id)
                with open(filename, 'rb') as f:
                    local_hash = md5(f.read()).hexdigest()
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

    def unpack(self, submission_id):
        try:
            if submission_id in self.download_dirs:
                download_dir = self.download_dir(submission_id)
            else:
                return False
            log.info("Unpacking %s..." % download_dir)
            with CD(download_dir):
                if platform.system() == 'Windows':
                    zip_files = [
                        ("entry.tar.gz", "7z x -obot -y entry.tar.gz > NUL"),
                        ("entry.tgz", "7z x -obot -y entry.tgz > NUL"),
                        ("entry.zip", "7z x -obot -y entry.zip > NUL")
                    ]
                else:
                    zip_files = [
                        ("entry.tar.gz", "mkdir bot; tar xfz entry.tar.gz -C bot > /dev/null 2> /dev/null"),
                        ("entry.tar.xz", "mkdir bot; tar xfJ entry.tar.xz -C bot > /dev/null 2> /dev/null"),
                        ("entry.tar.bz2", "mkdir bot; tar xfj entry.tar.bz2 -C bot > /dev/null 2> /dev/null"),
                        ("entry.txz", "mkdir bot; tar xfJ entry.txz -C bot > /dev/null 2> /dev/null"),
                        ("entry.tbz", "mkdir bot; tar xfj entry.tbz -C bot > /dev/null 2> /dev/null"),
                        ("entry.tgz", "mkdir bot; tar xfz entry.tgz -C bot > /dev/null 2> /dev/null"),
                        ("entry.zip", "unzip -u -dbot entry.zip > /dev/null 2> /dev/null")
                    ]
                for file_name, command in zip_files:
                    if os.path.exists(file_name):
                        exit_status = os.system(command)
                        log.info("unzip %s, status: %s"
                                % (file_name, exit_status))
                        if exit_status != 0:
                            return False
                        # remove __MACOSX directory
                        mac_path = os.path.join('bot', '__MACOSX')
                        if os.path.exists(mac_path) and os.path.isdir(mac_path):
                            shutil.rmtree(mac_path)
                        # check for single directory only and move everything up
                        unpacked_listing = [p for p in os.listdir('bot')]
                        if len(unpacked_listing) == 1:
                            one_path = os.path.join('bot', unpacked_listing[0])
                            if os.path.isdir(one_path):
                                os.rename(one_path, 'tmp')
                                shutil.rmtree('bot')
                                os.rename('tmp', 'bot')
                        for dirpath, _, filenames in os.walk("."):
                            os.chmod(dirpath, 0755)
                            for filename in filenames:
                                filename = os.path.join(dirpath, filename)
                                os.chmod(filename,stat.S_IMODE(os.stat(filename).st_mode) | stat.S_IRUSR | stat.S_IRGRP | stat.S_IROTH)
                        break
                else:
                    return False
                return True
        except:
            log.error(traceback.format_exc())
            return False

    def compile(self, submission_id=None, report_status=(False, False), run_test=True):
        report_success, report_failure = report_status
        def report(status, language="Unknown", errors=None):
            # oooh, tricky, a terinary in an if
            if report_success if type(errors) != list else report_failure:
                self.post_id += 1
                result = {"post_id": self.post_id,
                          "submission_id": submission_id,
                          "status_id": status,
                          "language": language }
                if status != 40:
                    if type(errors) != list:
                        errors = [errors] # for valid json
                    result['errors'] = json.dumps(errors)
                return self.cloud.post_result('api_compile_result', result)
            else:
                return True
        if submission_id == None:
            # compile in current directory
            compiler.compile_anything(os.getcwd())
        else:
            submission_dir = self.submission_dir(submission_id)
            if os.path.exists(submission_dir):
                log.info("Already compiled: %s" % submission_id)
                if run_test:
                    errors = self.functional_test(submission_id)
                else:
                    errors = None
                if errors == None:
                    if report(STATUS_RUNABLE, compiler.get_run_lang(submission_dir)):
                        return True
                    else:
                        log.debug("Cleanup of compiled dir: {0}".format(submission_dir))
                        shutil.rmtree(submission_dir)
                        return False
                else:
                    report(STATUS_TEST_ERROR, compiler.get_run_lang(submission_dir), errors)
                    log.debug("Cleanup of compiled dir: {0}".format(submission_dir))
                    shutil.rmtree(submission_dir)
                    return False
            if (not submission_id in self.download_dirs or
                len(os.listdir(self.download_dir(submission_id))) == 0):
                if not self.download_submission(submission_id):
                    report(STATUS_DOWNLOAD_ERROR)
                    log.error("Download Error")
                    return False
            download_dir = self.download_dir(submission_id)
            if not os.path.exists(os.path.join(self.download_dir(submission_id),
                                               'bot')):
                if len(os.listdir(download_dir)) == 1:
                    if not self.unpack(submission_id):
                        report(STATUS_UNPACK_ERROR)
                        log.error("Unpack Error")
                        return False
            log.info("Compiling %s " % submission_id)
            bot_dir = os.path.join(download_dir, 'bot')
            timelimit = 10 * 60 # 10 minute limit to compile submission
            if not run_test:
                # give it 50% more time if this isn't the initial compilation
                # this is to try and prevent the situation where the initial
                # compilation just makes it in the time limit and then a
                # subsequent compilation fails when another worker goes to
                # play a game with it
                timelimit += timelimit * 0.5
            detected_lang, errors = compiler.compile_anything(bot_dir,
                    timelimit)
            if errors != None:
                log.error(errors)
                if not self.debug:
                    shutil.rmtree(download_dir)
                log.error(detected_lang)
                report(STATUS_COMPILE_ERROR, detected_lang, errors=errors);
                log.error("Compile Error")
                return False
            else:
                log.info("Detected language: {0}".format(detected_lang))
                if not os.path.exists(os.path.split(submission_dir)[0]):
                    os.makedirs(os.path.split(submission_dir)[0])
                if run_test:
                    errors = self.functional_test(submission_id)
                else:
                    errors = None
                if errors == None:
                    os.rename(download_dir, submission_dir)
                    del self.download_dirs[submission_id]
                    if report(STATUS_RUNABLE, detected_lang):
                        return True
                    else:
                        # could not report back to server, cleanup compiled dir
                        log.debug("Cleanup of compiled dir: {0}".format(submission_dir))
                        shutil.rmtree(submission_dir)
                        return False
                else:
                    log.info("Functional Test Failure")
                    report(STATUS_TEST_ERROR, detected_lang, errors)
                    return False

    def get_map(self, map_filename):
        map_file = os.path.join(server_info["maps_path"], map_filename)
        if not os.path.exists(map_file):
            data = self.cloud.get_map(map_filename)
            if data == None:
                raise Exception("map", "Could not download map from main server.")
            map_dir = os.path.split(map_file)[0]
            if not os.path.exists(map_dir):
                os.makedirs(map_dir)
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
            f = open(os.path.join(server_info['repo_path'],
                                  'ants/submission_test/test.map'), 'r')
            self.test_map = f.read()
            f.close()
        return self.test_map

    def functional_test(self, submission_id):
        self.post_id += 1
        log.info("Running functional test for %s" % submission_id)
        options = copy(server_info["game_options"])
        options['strict'] = True # kills bot on invalid inputs
        options['food'] = 'none'
        options['turns'] = 30
        log.debug(options)
        options["map"] = self.get_test_map()
        options['capture_errors'] = True
        game = Ants(options)
        if submission_id in self.download_dirs:
            bot_dir = self.download_dirs[submission_id]
        else:
            bot_dir = self.submission_dir(submission_id)
        bots = [(os.path.join(bot_dir, 'bot'),
                 compiler.get_run_cmd(bot_dir)),
                (os.path.join(server_info['repo_path'],"ants","submission_test"), "python TestBot.py")]
        log.debug(bots)
        # set worker debug logging
        if self.debug:
            options['verbose_log'] = sys.stdout
            #options['stream_log'] = sys.stdout
            options['error_logs'] = [sys.stderr, sys.stderr]
            # options['output_logs'] = [sys.stdout, sys.stdout]
            # options['input_logs'] = [sys.stdout, sys.stdout]
        result = run_game(game, bots, options)
        if 'status' in result:
            if result['status'][1] in ('crashed', 'timeout', 'invalid'):
                if type(result['errors'][1]) == unicode:
                    errors_str = uni_to_ascii(result['errors'][1])
                else:
                    errors_str = '["'+ '","'.join(uni_to_ascii(e) for e in
                        result['errors'][1]) + '"]'
                msg = 'TestBot is not operational\n' + errors_str
                log.error(msg)
                return msg
            log.info(result['status'][0]) # player 0 is the bot we are testing
            if result['status'][0] in ('crashed', 'timeout', 'invalid'):
                if type(result['errors'][1]) == unicode:
                    errors_str = uni_to_ascii(result['errors'][0])
                else:
                    errors_str = '["'+ '","'.join(uni_to_ascii(e) for e in
                        result['errors'][0]) + '"]'
                log.info(errors_str)
                return result['errors'][0]
        elif 'error' in result:
            msg = 'Function Test failure: ' + result['error']
            log.error(msg)
            return msg
        return None

    def game(self, task, report_status=False):
        self.post_id += 1
        try:
            matchup_id = int(task["matchup_id"])
            log.info("Running game %s..." % matchup_id)
            options = None
            if 'game_options' in task:
                options = task["game_options"]
            if options == None:
                options = copy(server_info["game_options"])
            options["map"] = self.get_map(task['map_filename'])
            options["turns"] = task['max_turns']
            options["output_json"] = True
            game = Ants(options)
            bots = []
            for submission_id in task["submissions"]:
                submission_id = int(submission_id)
                # sometimes the Go bots get marked good,
                # then the Go language is updated and breaks syntax,
                # then they need to be marked as invalid again
                # so this will report status to turn off bots that fail
                #   sometime after they already succeeded
                if self.compile(submission_id, report_status=(False, True), run_test=False):
                    submission_dir = self.submission_dir(submission_id)
                    run_cmd = compiler.get_run_cmd(submission_dir)
                    #run_dir = tempfile.mkdtemp(dir=server_info["compiled_path"])
                    bot_dir = os.path.join(submission_dir, 'bot')
                    bots.append((bot_dir, run_cmd))
                    #shutil.copytree(submission_dir, run_dir)
                else:
                    self.clean_download(submission_id)
                    raise Exception('bot', 'Can not compile bot %s' % submission_id)
            options['game_id'] = matchup_id
            log.debug((game.__class__.__name__, task['submissions'], options, matchup_id))
            # set worker debug logging
            if self.debug:
                options['verbose_log'] = sys.stdout
                replay_log = open('replay.json', 'w')
                options['replay_log'] = replay_log
                #options['stream_log'] = sys.stdout
                options['error_logs'] = [sys.stderr for _ in range(len(bots))]
                # options['output_logs'] = [sys.stdout, sys.stdout]
                # options['input_logs'] = [sys.stdout, sys.stdout]
            options['capture_errors'] = True
            result = run_game(game, bots, options)
            if self.debug:
                replay_log.close()
            log.debug(result)
            if 'game_id' in result:
                del result['game_id']
            result['matchup_id'] = matchup_id
            result['post_id'] = self.post_id
            if report_status:
                return self.cloud.post_result('api_game_result', result)
        except Exception as ex:
            log.error(traceback.format_exc())
            result = {"post_id": self.post_id,
                      "matchup_id": matchup_id,
                      "error": traceback.format_exc() }
            success = self.cloud.post_result('api_game_result', result)
            # cleanup download dirs
            map(self.clean_download, map(int, task['submissions']))
            return success

    def task(self, last=False):
        task = self.cloud.get_task()
        if task:
            try:
                log.info("Received task: %s" % task)
                if task['task'] == 'compile':
                    submission_id = int(task['submission_id'])
                    try:
                        if not self.compile(submission_id, (True, True)):
                            self.clean_download(submission_id)
                        return True
                    except Exception:
                        log.error(traceback.format_exc())
                        self.clean_download(submission_id)
                        return False
                elif task['task'] == 'game':
                    return self.game(task, True)
                else:
                    if not last:
                        time.sleep(20)
                    # prevent worker from stopping on unknown tasks
                    return True
            except:
                log.error('Task Failure')
                log.error(traceback.format_exc())
                quit()
        else:
            log.error("Error retrieving task from server.")

def main(argv):
    usage ="""Usage: %prog [options]\nThe worker will not attempt to retrieve
    tasks from the server if a specifec submission_id is given."""
    parser = OptionParser(usage=usage)
    parser.add_option("-s", "--submission_id", dest="submission_id",
                      type="int", default=0,
                      help="Submission id to use for hash, download and compile")
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
    parser.add_option("--debug", dest="debug",
                      action="store_true", default=False,
                      help="Set the log level to debug")

    (opts, _) = parser.parse_args(argv)
    if opts.debug:
        log.setLevel(logging.DEBUG)
        worker = Worker(True)
    else:
        worker = Worker()

    # if the worker is not run in task mode, it will not clean up the download
    #    dir, so that debugging can be done on what had been downloaded/unzipped

    # download and compile
    if opts.submission_id != 0 and opts.compile:
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
        if os.path.exists('last_post.json'):
            log.warning("Last result was not sent successfully, resending....")
            result = None
            with open('last_post.json') as f:
                try:
                    method, result = json.loads(f.read())
                except:
                    log.warning("Last game result file can't be read")
            if result != None:
                if not worker.cloud.post_result(method, result):
                    return False
            else:
                os.remove('last_post.json')
        if opts.num_tasks <= 0:
            try:
                script_loc = os.path.realpath(os.path.dirname(__file__))
                while True:
                    log.info("Getting task infinity + 1")
                    if not worker.task():
                        log.warning("Task failed, stopping worker")
                        break
                    print()
                    if os.path.exists(os.path.join(script_loc, "stop_worker")):
                        log.info("Found worker stop file, exiting.")
                        break
            except KeyboardInterrupt:
                log.info("[Ctrl] + C, Stopping worker")
        else:
            for task_count in range(opts.num_tasks):
                log.info("Getting task %s" % (task_count + 1))
                worker.task((task_count+1)==opts.num_tasks)
                print()
        return

    parser.print_help()

if __name__ == '__main__':
    main(sys.argv[1:])
