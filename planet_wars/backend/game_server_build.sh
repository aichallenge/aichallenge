#!/bin/sh
# 
# This should build a cloud game server.
# 
# Start with ami-6836dc01 Ubuntu 8 HARDY LTS

# If manualy using the script, uncomment the two below lines

# api_base_url="http://ai-contest.com"
# api_key="KEYGOESHERElakfjsdkjfhalksdjfhalksdjfhkdasjhfaklj"

set -e -x # print commands to output - helpful for debugging

# install all needed packages
export DEBIAN_FRONTEND=noninteractive
aptitude update
aptitude install -y openssh-server curl
aptitude install -y htop subversion screen rrdtool collectd unzip pwgen vim
aptitude install -y mysql-server mysql-client
echo $?
aptitude install -y python2.5-mysqldb python2.5-simplejson
echo $?
aptitude install -y ruby1.9 php5-cli perl gcc g++ libssl-dev make glibc-2.7-1 common-lisp-controller ghc6 git-core haskell-utils ocaml openjdk-6-jre-headless sbcl libboost-dev
echo $?

# add badgerports for latest mono
if ! grep -q 'deb http://badgerports.org/ hardy main' /etc/apt/sources.list
then
  curl 'http://badgerports.org/directhex.ppa.asc' | apt-key add -
  echo 'deb http://badgerports.org/ hardy main ' >> /etc/apt/sources.list
fi

aptitude update
aptitude install -y mono-2.0-devel
echo $?

#needed for golang build
aptitude install -y bison ed gawk libc6-dev make
echo $?
aptitude install -y python-setuptools python-dev build-essential
echo $?
easy_install mercurial

export DEBIAN_FRONTEND=''

#install golang
if [ ! -e /usr/local/bin/godoc ]
then
  if [ ! -e /usr/local/src ]
  then
    mkdir /usr/local/src
  fi
  cd /usr/local/src
  
  if [ ! -e /usr/local/src/go ]
  then
    hg clone -r release.2010-10-20 https://go.googlecode.com/hg/ go
    echo 'export GOROOT=/usr/local/src/go' >> /root/.bashrc
    echo 'export GOBIN=/usr/local/bin' >> /root/.bashrc
  fi
  
  #export this, in case we're running the script a second time and .bashrc hasn't been called
  export GOBIN=/usr/local/bin
  cd /usr/local/src/go/src
  ./all.bash
fi

# install node.js
if [ ! -e /usr/local/bin/node ]
then
  cd /root/
  curl 'http://nodejs.org/dist/node-v0.2.2.tar.gz' | tar -xz \
  && cd node-v0.2.2/ \
  && ./configure && make && make install
fi

# set default ruby to ruby 1.9
if [ ! `readlink /usr/bin/ruby` = "/usr/bin/ruby1.9" ]
then
  test -e /usr/bin/ruby && mv /usr/bin/ruby /usr/bin/ruby_old
  ln -s /usr/bin/ruby1.9 /usr/bin/ruby
fi

echo 'export HISTCONTROL=erasedups' >> /root/.bashrc
echo 'export HISTSIZE=10000' >> /root/.bashrc
echo 'shopt -s histappend' >> /root/.bashrc
echo 'PROMPT_COMMAND=\"history -a\"' >> /root/.bashrc

# create and setup user 'ubuntu'
if [ ! -e /home/ubuntu/ ]
then
  adduser ubuntu --disabled-password --gecos ""
  adduser ubuntu admin
fi
if ! grep -q '^ubuntu ALL=(ALL) NOPASSWD:ALL' /etc/sudoers
then
  chmod 640 /etc/sudoers
  echo 'ubuntu ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers
  chmod 440 /etc/sudoers
fi
if [ ! -e /home/ubuntu/.ssh/ ]
then mkdir /home/ubuntu/.ssh; chown ubuntu:ubuntu /home/ubuntu/.ssh; fi
AUTHKEY_FILE=/home/ubuntu/.ssh/authorized_keys
if [ ! -e $AUTHKEY_FILE]
then
  touch $AUTHKEY_FILE
  chown ubuntu:ubuntu $AUTHKEY_FILE
  chmod 600 $AUTHKEY_FILE
fi
if ! grep -q ' contest@ai-contest$' $AUTHKEY_FILE
then
  echo 'ssh-rsa AAAAB3NzaC1yc2EAAAABIwAAAQEAsLlstx9E6TiZZ1InY/d6r1Ykucjvcj0whesjURGstGgVcUhAmFC9EqheV7QniORpJFwBA7qBL00K1uGjiYpn4ykW6pPJMpw1cMztXA6u9ByejchUHPAbn5vy1e+1kxQsttTMe95rAYrdG91s1Uu0o4wxStluK90zdTukOX46Qjw5tcD7RujNoYCJnMWwWL2BYc+C1A1UeX6nrFlJJjLCgP1sb24BBCPgMoBnUCbH5+xPSaLKdQoAGicya6rVcsYLZqSxIOyQ43YHNXEEckYBxzRSMKJj5JzM7rhMNUb1HpsGZgxIVy74mCVRyO29TtlMrKZFjvcVIdt/UktG2lRETw== contest@ai-contest' >> $AUTHKEY_FILE
fi

adduser contest --disabled-password --gecos ""

cd /home/contest/; svn checkout https://ai-contest.googlecode.com/svn/branches/20100929-games-in-the-cloud ai-contest

/etc/init.d/mysql start

# Amazon specific. Disallow access to instance userdata (since it would contain the api registration key)
route add -host 169.254.169.254 reject

# Copy over the latest scripts
cd /home/contest/ai-contest/planet_wars/backend/
    echo '
server_info = {
  "db_username" : "root",
  "db_password" : "",
  "db_name" : "contest",
  "db_host" : "127.0.0.1",
  "mail_username" : "donotreply@ai-contest.com",
  "mail_name" : "AI Contest",
  "mail_password" : ""
}
    ' > server_info.py
    echo "server_info['api_base_url']='$api_base_url'" >> server_info.py
    echo "server_info['api_key']='$api_key'" >> server_info.py
    chmod 600 server_info.py
    echo 'create database contest' | mysql
    mysql contest < schema.sql
    python create_jail_users.py 32
    iptables-save > /etc/iptables.rules
    chown -R contest:contest .
    chmod 640 /etc/sudoers
    echo 'contest ALL = (%jailusers) NOPASSWD: ALL' >> /etc/sudoers
    chmod 440 /etc/sudoers
cd /home/contest/ai-contest/planet_wars/submissions/; chown -R contest:contest .

echo '#!/bin/sh
iptables-restore < /etc/iptables.rules
exit 0
' > /etc/network/if-pre-up.d/iptablesload
chmod +x /etc/network/if-pre-up.d/iptablesload

cd /home/contest/ai-contest/planet_wars/backend/

if [ ! -e /etc/cron.d/ai-games ]
then
echo '@reboot root /home/contest/ai-contest/planet_wars/backend/start_worker.sh' > /etc/cron.d/ai-games
fi

/home/contest/ai-contest/planet_wars/backend/start_worker.sh

# To run a single game, to check that it is working
# sudo -u contest python tournament_manager.py 1

