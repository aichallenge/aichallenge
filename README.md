AI Challenge Source Code Repository
===================================

*Codename: Epsilon*

This code will provide the basis for the next contest (Winter 2011).
Epsilon is supposed to implement the ants game.

Folder Contents
---------------

* `ants/` - Everything related to ants: engine, starter packages, maps/mapgen, visualizer
* `integration_testing` - Anything related to testing/stress-testing the contest setup
* `manager/` - Tournament manager which coordinates the workers and computes skill ratings
* `worker/` - Standalone workers which run games (including compiler and sandbox)
* `sql/` - Files for creating an empty sql database
* `website/` - Main website and frontend

Installation
---------------

* Install Latest Ubuntu

* Install Dependencies:

`apt-get install subversion apache2 mysql-server php5 libapache2-mod-php5 php5-mysql python-mysqldb openssh-server build-essential`

* For java bots `apt-get install openjdk-6-jre-headless openjdk-6-jdk`

* Create mysql aichallenge user and database

Schema files are located in `sql/`, from that folder execute

`mysql -u $aichallenge_user -p'$db_password' $aichallenge_db < * .sql`

* Configure `www/server_info.php`, sample file in `www/server_info.php.sample`

* Configure `manager/server_info.py`, sample file in `manager/server_info.py.sample`

*  Create submissions directory and give `www-data` all permissions

Change `www/check_submit.php` and `manager/compile_daemon.py` to point to the submissions directory

* Regenerate jail users ssh key then create the users with:

`sudo python create_jail_users.py 10`

* Make sure private key `jail_id_rsa` has restricted permissions

* Setup apache to serve up `www/` directory.

* Bump hard nproc limit up to 15 so java bots can run in `/etc/security/limits.conf`