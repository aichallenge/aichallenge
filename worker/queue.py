import time
from boto.exception import SQSError
from boto.sqs.connection import SQSConnection
from boto.sqs.jsonmessage import JSONMessage
from worker.config import config

class SQSQueue(object):
    """message queue which uses Amazon SQS"""
    def __init__(self, queue_name, message_lifetime=300, logger=None):
        access_key = config.get('worker', 'aws_access_key')
        secret_key = config.get('worker', 'aws_secret_key')
        self.sqs = SQSConnection(access_key, secret_key)
        self.queue = self.sqs.create_queue(queue_name, message_lifetime)
        self.queue.set_message_class(JSONMessage)
        self.logger = logger
    
    def get(self):
        """synchronously get a single message from the queue"""
        try:
            message = self.queue.read()
            while not message:
                time.sleep(3)
                message = self.queue.read()
        
            return message
        except SQSError as e:
            if self.logger is not None:
                self.logger.warn('SQSError: ' + str(e))
            time.sleep(3)
            return self.get()
    
    def delete(self, message):
        """remove the message from the queue"""
        self.queue.delete_message(message)
    
    def size(self):
        """return the (approximate) size of the queue"""
        return self.queue.count()
    
    def extend_lease(self, message):
        """extend the lease on a message"""
        self.queue.change_message_visibility(message, 300)
    
    def put(self, body):
        """add a message to the queue"""
        message = JSONMessage(self.queue, body)
        self.queue.write(message)
