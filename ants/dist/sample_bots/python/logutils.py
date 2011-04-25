import logging

"""Initializes Logging infrastructure for the bot"""

def initLogging():
  #logLevel = logging.DEBUG
  logLevel = logging.INFO
  #logLevel = logging.WARNING
  logger = logging.getLogger("ConsoleLog")
  logger.setLevel(logLevel)

  ch = logging.StreamHandler()
  ch.setLevel(logLevel)

  formatter = logging.Formatter("%(asctime)s-%(version)s- Turn:%(turn_number)s-%(funcName)25s() - %(message)s")
  ch.setFormatter(formatter)
  getLogger().addHandler(ch)


def getLogger():
  return logging.getLogger("ConsoleLog")
