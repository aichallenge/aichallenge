import ConfigParser
import os
import os.path

if 'AICHALLENGE_PREFIX' not in os.environ:
    os.environ['AICHALLENGE_PREFIX'] = ''

config = ConfigParser.SafeConfigParser()
config.read(os.path.expandvars('$AICHALLENGE_PREFIX/etc/aichallenge.conf'))
