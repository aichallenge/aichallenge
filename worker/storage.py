import boto.s3
import cStringIO
import os
import os.path
import time
import tarfile
import zipfile

from worker.config import config

def fetch(key, logger=None):
    """download and extract an archive"""
    
    template = "$AICHALLENGE_PREFIX/var/lib/aichallenge/submissions/%s"
    path = os.path.expandvars(template % key)
    if os.path.isdir(path):
        # already decompressed -- move on
        return
    
    if logger is not None:
        logger.info('downloading ' + key)
    
    access_key = config.get('worker', 'aws_access_key')
    secret_key = config.get('worker', 'aws_secret_key')
    bucket = config.get('worker', 'submission_bucket')
    prefix = config.get('worker', 'submission_prefix')
    s3 = boto.s3.Connection(access_key, secret_key)
    bucket = s3.get_bucket(bucket)
    s3_key = bucket.get_key(prefix + key)
    
    if s3_key is None:
        raise KeyError
    
    io = cStringIO.StringIO()
    s3_key.get_contents_to_file(io)
    
    try:
        io.seek(0)
        zip = zipfile.ZipFile(io)
        decompress_zip(zip, path)
        return
    except zipfile.BadZipfile:
        pass
    
    try:
        io.seek(0)
        tar = tarfile.open(fileobj=io)
        decompress_tar(tar, path)
        return
    except tarfile.TarError:
        pass
    
    raise ValueError, "invalid archive"

def decompress_zip(zip, path):
    path_tmp = path + "." + str(time.time())
    os.mkdir(path_tmp, 0750)
    for member in zip.namelist():
        if not member.startswith("/") and not member.startswith(".."):
            zip.extract(member, path_tmp)
    
    try:
        os.rename(path_tmp, path)
    except OSError:
        pass # someone else probably did it at the same time

def decompress_tar(tar, path):
    path_tmp = path + "." + str(time.time())
    os.mkdir(path_tmp, 0750)
    for member in tar.getmembers():
        name_ok = (not member.name.startswith("/") and
                    not member.name.startswith(".."))
        type_ok = member.isfile() or member.isdir()
        if name_ok and type_ok:
            tar.extract(member, path_tmp)    
    
    try:
        os.rename(path_tmp, path)
    except OSError:
        pass # someone else probably did it at the same time
