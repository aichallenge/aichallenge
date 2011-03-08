import os
import json
import urllib
import tempfile
import logging

from server_info import server_info
from submission_hash import hash_file_sha

# Set up logging
logger = logging.getLogger('tm_logger')
logger.setLevel(logging.INFO)
handler = logging.handlers.RotatingFileHandler("tm.log",
                                               maxBytes=1000000,
                                               backupCount=5)
logger.addHandler(handler)
tm_pid = os.getpid()

def log_message(message):
    logger.info(str(tm_pid) + ": %s" % message)
    print message
  
class GameAPIClient:
    def __init__(self, base_url, api_key):
        self.base_url = base_url
        self.api_key = api_key
    
    def get_url(self, method):
        return '%s/%s.php?api_key=%s' % (self.base_url, method, self.api_key)

    def get_task(self):
        url = self.get_url('api_get_task')
        data = urllib.urlopen(url).read()
        return json.loads(data)
    
    def get_submission_hash(self, submission_id):
        url = self.get_url('api_get_submission_hash')
        url += '&submission_id=%s' % submission_id
        data = urllib.urlopen(url).read()
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

    def compile_submission(self):
      log_message("Compiling %s " % submission_id)
      main_dir = os.getcwd()
      os.chdir(download_dir)
      compile_log = Log()
      success = compile_function(language_name, compile_log)
      os.chdir(main_dir)
      if not success:
        shutil.rmtree(download_dir)
        log_message(compile_log.err)
        log_message("ERROR: could not compile submission")
        raise Exception()

    def ensure_submission_is_local(self, submission_id):
        submission_dir = os.path.join(server_info["submissions_path"], str(submission_id))
        if os.path.exists(submission_dir):
            return
        else:
            download_dir = tempfile.mkdtemp(dir=server_info["submissions_path"])
            os.chmod(download_dir, 0755)
            filename = self.get_submission(submission_id, download_dir)
            remote_hash = self.get_submission_hash(submission_id)
            local_hash = hash_file_sha(filename)
            if local_hash != remote_hash:
                log_message("After downloading submission %s to %s hash didn't match" %
                        (submission_id, download_dir))
                raise Exception()
        
        
def compile():
    

def game():
    pass


def main():
    cloud = GameAPIClient( server_info['api_base_url'], server_info['api_key'])
    
    while True:
        task = cloud.get_task()
        if task['task'] == 'compile':
            compile(int(task['submission_id']))
        elif task['task'] == 'game':
            game(task)

if __name__ == '__main__':
    main()