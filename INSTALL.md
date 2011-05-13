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

* Setup visualizer, from the ants/visualizer folder:
    * Extra packages to support build: `sudo apt-get install cvs openjdk-6-jdk ant icedtea-plugin`
        * cvs is needed to download and install rhino, javascript on jvm
    * `sudo ant deploy -Djava.plugin=$plugin -Ddeploy.path=$websiteroot`
    * $plugin being different between ubuntu versions:
        * 10.10: `/usr/lib/jvm/java-6-openjdk/jre/lib/plugin.jar` requiring the `openjdk-6-jre-lib` package
        * 11.04: `/usr/share/icedtea-web/plugin.jar` requiring the `icedtea-plugin` package
* Install the content on the website, cd to the website dir:
    
    git clone git://github.com/aichallenge/aichallenge.wiki.git
    ./setup.py

* Install the starter packs, TODO
    
* Configure email sending, change website/server_info.php

    * set "mailer_address" => "donotsend" if you do not want emails