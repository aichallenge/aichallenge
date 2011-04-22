## Server

* Install Ubuntu 11.10
* Install git and a few of your favorite admin tools
* Create the `contest` user
* `git clone` the repository inside
* Initialize the git submodules, see the README for more details.
* `sudo python setup/server_setup.py`

    * Leave blank for root password (TODO: Fix this) if there's no mysql installed (change it later after install).
    * Website hostname option is used by the apache host setup, do not include port in there.
    
* Configure email sending, change website/server_info.php

    * set "mailer_address" => "donotsend" if you do not want emails
    
* Change `www/check_submit.php` and `manager/compile_daemon.py` to point to the submissions directory. TODO: automate this

## Worker

* For java bots `apt-get install openjdk-6-jre-headless openjdk-6-jdk`
* Create the users with:

`sudo python create_jail_users.py 10`