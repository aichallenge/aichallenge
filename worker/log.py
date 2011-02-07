import logging
import os
import os.path

def setup():
    """setup the logger for this process"""
    filename = '$AICHALLENGE_PREFIX/var/log/aichallenge/worker.%d.log'
    filename = os.path.expandvars(filename % os.getpid())
    logger = logging.getLogger(filename)
    logger.setLevel(logging.INFO)
    
    format = "%(asctime)s  %(levelname)s worker[%(process)d]  %(message)s"
    handler = logging.handlers.RotatingFileHandler(filename, maxBytes=5e7,
                                                   backupCount=3)
    handler.setFormatter(logging.Formatter(format))
    logger.addHandler(handler)
    
    return logger
