#!/usr/bin/python

import docker
client = docker.from_env()

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


if __name__ == '__main__':
    cloud = GameAPIClient( server_info['api_base_url'], server_info['api_key'])
