## Server

* Install Ubuntu 11.04
* Install git and a few of your favorite admin tools
* Create the `contest` user
    For Ubuntu: `sudo useradd -d /home/contest -m -s /bin/bash contest`
* Switch to contest user: `sudo su contest; cd ~`
* `git clone` the repository inside
* Initialize the git submodules. `git submodule init; git submodule update`
* `sudo python setup/server_setup.py`

    * Leave blank for root mysql password (TODO: Fix this) if there's no mysql installed (change it later after install).
    * Website hostname option is used by the apache host setup, do not include port in there.
