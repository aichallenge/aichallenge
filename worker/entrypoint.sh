#!/bin/bash

set -e
set -x

# echo "Going to download worker files at "
# curl http://${API_HOSTNAME}/api_server_setup.php?api_create_key=${API_CREATE_KEY} | bash
curl -v http://${API_HOSTNAME}/api_server_setup.php | bash


wget http://${API_HOSTNAME}/api_server_setup.php something.sh
cat something.sh
bash something.sh