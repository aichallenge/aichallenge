#!/bin/sh
# 
# This should build a cloud game server.
# 
# Start with ami-5d59be34 Ubuntu 8 HARDY LTS

# If manualy using the script, uncomment the two below lines

# api_base_url="http://ai-contest.com"
# api_key="KEYGOESHERElakfjsdkjfhalksdjfhalksdjfhkdasjhfaklj"



set -e -x # print commands to output - helpful for debugging

export DEBIAN_FRONTEND=noninteractive
aptitude update
aptitude install -y htop subversion curl screen rrdtool collectd unzip pwgen vim
aptitude install -y mysql-server mysql-client
echo $?
aptitude install -y python2.5-mysqldb python2.5-simplejson
echo $?
aptitude install -y python ruby1.9 php5-cli perl gcc g++ libssl-dev make glibc-2.7-1 common-lisp-controller ghc6 git-core haskell-utils mono-common mono-gac mono-gmcs mono-jit mono-mcs mono-runtime mono-utils ocaml openjdk-6-jre-headless sbcl libboost-dev python2.5-numpy
echo $?
export DEBIAN_FRONTEND=''

cd /root/
    curl 'http://nodejs.org/dist/node-v0.2.2.tar.gz' | tar -xz \
    && cd node-v0.2.2/ \
    && ./configure && make && make install

adduser contest --disabled-password --gecos ""
echo 'export HISTCONTROL=erasedups' >> /root/.bashrc
echo 'export HISTSIZE=10000' >> /root/.bashrc
echo 'shopt -s histappend' >> /root/.bashrc
echo 'PROMPT_COMMAND=\"history -a\"' >> /root/.bashrc
mv /usr/bin/ruby /usr/bin/ruby_old
ln -s /usr/bin/ruby1.9 /usr/bin/ruby


cd /home/contest/; svn checkout https://ai-contest.googlecode.com/svn/branches/20100929-games-in-the-cloud ai-contest --username danielvf
/etc/init.d/mysql start

# Amazon specific. Disallow access to instance userdata (since it would contain the api registration key)
route add -host 169.254.169.254 reject 

# Copy over the latest scripts

cd /home/contest/ai-contest/planet_wars/backend/; 
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
    chown -R contest:contest .
    chmod 640 /etc/sudoers
    echo 'contest ALL = (%jailusers) NOPASSWD: ALL' >> /etc/sudoers
    chmod 440 /etc/sudoers
cd /home/contest/ai-contest/planet_wars/submissions/; chown -R contest:contest .



cd /home/contest/ai-contest/planet_wars/backend/;


# First run a single game, to check that it is working
# sudo -u contest python tournament_manager.py 1

# Run games by launching screen, then two instances of
# for (( ; ; )); do   sudo -u contest python tournament_manager.py 575 > /dev/null 2> /dev/null; sleep 5; done


echo '@reboot /home/contest/ai-contest/planet_wars/backend/tournament_manager_runner.sh &' >> /etc/cron.d/ai-games
echo '@reboot /home/contest/ai-contest/planet_wars/backend/tournament_manager_runner.sh &' >> /etc/cron.d/ai-games
/home/contest/ai-contest/planet_wars/backend/tournament_manager_runner.sh &
/home/contest/ai-contest/planet_wars/backend/tournament_manager_runner.sh &