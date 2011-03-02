#!/bin/sh
# 
# This should build a cloud game server and is designed to run as root on a
# fresh Ubuntu 10.10 install.
#
# If manually using the script, uncomment the two lines below

# api_base_url="http://ai-contest.com"
# api_key="KEYGOESHERElakfjsdkjfhalksdjfhalksdjfhkdasjhfaklj"

set -e -x # print commands to output - helpful for debugging

# install all needed packages
export DEBIAN_FRONTEND=noninteractive
aptitude update
aptitude install -y openssh-server curl
aptitude install -y htop screen rrdtool collectd unzip pwgen vim git
aptitude install -y mysql-server mysql-client python-mysqldb python-simplejson
echo $?
aptitude install -y ruby1.9 php5-cli perl gcc g++ libssl-dev make common-lisp-controller haskell-platform ocaml openjdk-6-jdk sbcl libboost-dev mono-2.0-devel
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
    hg clone -r release.2011-02-24 https://go.googlecode.com/hg/ go
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
  curl 'http://nodejs.org/dist/node-v0.4.1.tar.gz' | tar -xz \
  && cd node-v0.4.1/ \
  && ./configure && make && make install
fi

# install groovy
if [ ! -e /usr/bin/groovy ]
then
  cd /root/
  curl 'http://dist.groovy.codehaus.org/distributions/installers/deb/groovy_1.7.8-1_all.deb' > groovy_1.7.8-1_all.deb
  dpkg -i groovy_1.7.8-1_all.deb
fi

# install scala
if [ ! -e /usr/bin/scala ]
then
  cd /root/
  curl "http://www.scala-lang.org/downloads/distrib/files/scala-2.8.1.final.tgz" > scala-2.8.1.final.tgz
  tar xzf scala-2.8.1.final.tgz
  mv scala-2.8.1.final /usr/share/scala
  ln -s /usr/share/scala/bin/scala /usr/bin/scala
  ln -s /usr/share/scala/bin/scalac /usr/bin/scalac
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

if [ ! -e /home/contest/ ]
then
adduser contest --disabled-password --gecos ""
fi

cd /home/contest/
if [ ! -e /home/contest/aichallenge ]
then
git clone git://github.com/aichallenge/aichallenge.git
else
cd aichallenge
git pull
fi

# Amazon specific. Disallow access to instance userdata (since it would contain the api registration key)
# route add -host 169.254.169.254 reject

SUBMISSION_DIR=/home/contest/submissions
if [ ! -e $SUBMISSION_DIR ]
then
mkdir $SUBMISSION_DIR
chown -R contest:contest $SUBMISSION_DIR
fi

MAP_DIR=/home/contest/maps
if [ ! -e $MAP_DIR ]
then
mkdir $MAP_DIR
chown -R contest:contest $MAP_DIR
fi

cd /home/contest/aichallenge/worker/

# Copy over the latest scripts
if [ ! -e /home/contest/aichallenge/worker/server_info.py ]
then
    echo '
server_info = {
  "db_username" : "root",
  "db_password" : "",
  "db_name" : "contest",
  "db_host" : "127.0.0.1",
  "mail_username" : "donotreply@ai-contest.com",
  "mail_name" : "AI Contest",
  "mail_password" : "",
  "root_path" : "/home/contest/aichallenge/",
  "maps_path" : "'$MAP_DIR'",
  "submissions_path" : "'$SUBMISSION_DIR'",
  "api_base_url" : "'$api_base_url'",
  "api_key" : "'$api_key'"
}
    ' > server_info.py
    chmod 600 server_info.py
fi

if ! mysql contest
then
echo 'create database contest' | mysql
mysql contest < schema.sql
fi

if [ ! -e /home/jailuser1 ]
then
python create_jail_users.py 32
chmod 640 /etc/sudoers
echo 'contest ALL = (%jailusers) NOPASSWD: ALL' >> /etc/sudoers
chmod 440 /etc/sudoers
iptables-save > /etc/iptables.rules
fi

if [ ! -e /etc/network/if-pre-up.d/iptablesload ]
then
echo '#!/bin/sh
iptables-restore < /etc/iptables.rules
exit 0
' > /etc/network/if-pre-up.d/iptablesload
chmod +x /etc/network/if-pre-up.d/iptablesload
fi

chown -R contest:contest /home/contest/aichallenge/worker

if [ ! -e /etc/cron.d/ai-games ]
then
echo '@reboot root /home/contest/aichallenge/worker/start_worker.sh' > /etc/cron.d/ai-games
fi

/home/contest/aichallenge/worker/start_worker.sh

# To run a single game, to check that it is working
# sudo -u contest python tournament_manager.py 1

