#!/usr/bin/python

import os.path
import re
import sys
from subprocess import Popen

CONTEST_PUBLIC_KEY = "ssh-rsa AAAAB3NzaC1yc2EAAAABIwAAAQEAsLlstx9E6TiZZ1InY/d6r1Ykucjvcj0whesjURGstGgVcUhAmFC9EqheV7QniORpJFwBA7qBL00K1uGjiYpn4ykW6pPJMpw1cMztXA6u9ByejchUHPAbn5vy1e+1kxQsttTMe95rAYrdG91s1Uu0o4wxStluK90zdTukOX46Qjw5tcD7RujNoYCJnMWwWL2BYc+C1A1UeX6nrFlJJjLCgP1sb24BBCPgMoBnUCbH5+xPSaLKdQoAGicya6rVcsYLZqSxIOyQ43YHNXEEckYBxzRSMKJj5JzM7rhMNUb1HpsGZgxIVy74mCVRyO29TtlMrKZFjvcVIdt/UktG2lRETw== contest@ai-contest"

class Environ(object):
    def __init__(self, var, value):
        self.env_var = var
        self.new_value = value

    def __enter__(self):
        self.start_value = os.environ.get(self.env_var, None)
        os.environ[self.env_var] = self.new_value
        return self.new_value

    def __exit__(self, type, value, traceback):
        if self.start_value is not None:
            os.environ[self.env_var] = self.start_value
        else:
            del os.environ[self.env_var]

class CD(object):
    def __init__(self, new_dir):
        self.new_dir = new_dir

    def __enter__(self):
        self.org_dir = os.getcwd()
        os.chdir(self.new_dir)
        return self.new_dir

    def __exit__(self, type, value, traceback):
        os.chdir(self.org_dir)

def file_matches(filename, line_pattern):
    regex = re.compile(line_pattern)
    with open(filename, 'r') as src:
        for line in src:
            if regex.search(line):
                return True
    return False

def append_line(filename, line):
    with open(filename, "a+") as afile:
        afile.write(line + '\n')

class CmdError(StandardError):
    def __init__(self, cmd, returncode):
        self.cmd = cmd
        self.returncode = returncode
        StandardError.__init__(self, "Error %s returned from %s"
            % (returncode, cmd))

def run_cmd(cmd):
    """ Run a command in a shell """
    print "Executing:", cmd
    proc = Popen(cmd, shell=True)
    status = proc.wait()
    if status != 0:
        raise CmdError(cmd, status)

def install_apt_packages(packages):
    """ Install system packages using aptitude """
    apt_cmd = "aptitude install -y "
    try:
        cmd = apt_cmd + packages
    except TypeError:
        cmd = apt_cmd + " ".join(packages)
    run_cmd(cmd)

def install_required_packages():
    """ This installs the packages that are required to run the worker scripts
    """
    pkg_list = ["curl", "unzip", "mysql-server", "mysql-client",
                "python-mysqldb"]
    install_apt_packages(pkg_list)

def install_utility_packages():
    """ This installs packages that are nice to have on workers but not
        required for correct operation """
    pkg_list = ["openssh-server", "htop", "screen", "vim", "git"]
    install_apt_packages(pkg_list)

def install_basic_languages():
    """ Install base set of submission languages,
        currently C, C++, Java and Python """
    pkg_list = ["gcc", "g++", "openjdk-6-jdk", "python-dev"]
    install_apt_packages(pkg_list)

def install_extra_packaged_languages():
    """ Install all extra languages that are part of the Ubuntu distribution
        and don't require any special installation steps """
    pkg_list = ["ruby1.9.1", "php5-cli", "perl", "haskell-platform", "ocaml",
            "common-lisp-controller", "sbcl", "mono-2.0-devel"]
    install_apt_packages(pkg_list)
    if not os.path.exists("/usr/bin/ruby"):
        os.symlink("/usr/bin/ruby1.9.1", "/usr/bin/ruby")

def install_golang():
    """ Install golang from a mercurial release """
    RELEASE_TAG = "release.2011-02-24"
    if os.path.exists("/usr/local/bin/godoc"):
        return
    pkg_list = ["bison", "ed", "gawk", "libc6-dev", "make",
            "python-setuptools", "build-essential", "mercurial"]
    install_apt_packages(pkg_list)
    try:
        os.makedirs("/usr/local/src")
    except OSError:
        pass
    with CD("/usr/local/src"):
        run_cmd("hg clone -r %s https://go.googlecode.com/hg/ go"
            % (RELEASE_TAG,))
    append_line("/root/.bashrc", "export GOROOT=/usr/local/src/go")
    append_line("/root/.bashrc", "export GOBIN=/usr/local/bin")
    with CD("/usr/local/src/go/src"):
        run_cmd("export GOBIN=/usr/local/bin; ./all.bash")

def install_nodejs():
    """ Install node.js """
    if os.path.exists("/usr/local/bin/node"):
        return
    install_apt_packages("make")
    try:
        os.makedirs("/usr/local/src/nodejs")
    except OSError:
        pass
    with CD("/usr/local/src/nodejs"):
        run_cmd("curl 'http://nodejs.org/dist/node-v0.4.1.tar.gz' | tar -xz")
    with CD("/usr/local/src/nodejs/node-v0.4.1"):
        run_cmd("./configure")
        run_cmd("make")
        run_cmd("make install")

def install_groovy():
    """ Install the Groovy language """
    if os.path.exists("/usr/bin/groovy"):
        return
    with CD("/root"):
        run_cmd("curl 'http://dist.groovy.codehaus.org/distributions/installers/deb/groovy_1.7.8-1_all.deb' > groovy_1.7.8-1_all.deb")
        run_cmd("dpkg -i groovy_1.7.8-1_all.deb")

def install_scala():
    """ Install the Scala language """
    if os.path.exists("/usr/bin/scala"):
        return
    with CD("/root"):
        run_cmd("curl 'http://www.scala-lang.org/downloads/distrib/files/scala-2.8.1.final.tgz' | tar xz")
        os.rename("scala-2.8.1.final", "/usr/share/scala")
        os.symlink("/usr/share/scala/bin/scala", "/usr/bin/scala")
        os.symlink("/usr/share/scala/bin/scalac", "/usr/bin/scalac")

def install_all_languages():
    install_basic_languages()
    install_extra_packaged_languages()
    install_golang()
    install_nodejs()
    install_groovy()
    install_scala()

def setup_contest_user():
    """ Setup the contest user that all worker scripts run under """
    if not os.path.exists("/home/contest"):
        run_cmd('adduser --disabled-password --gecos "" contest')

SERVER_INFO_TEMPLATE = """
server_info = {
  "db_username": "root",
  "db_password": "",
  "db_name": "worker",
  "db_host": "127.0.0.1",
  "mail_username": "donotreply@ai-contest.com",
  "mail_name": "AI Contest",
  "mail_password": "",
  "root_path": "%s",
  "maps_path": "%s",
  "submissions_path": "%s",
  "api_base_url": "%s",
  "api_key": "%s"
}
"""

def setup_contest_files(contest_root, api_url, api_key):
    """ Setup all the contest specific files and directories """
    with CD(contest_root):
        if not os.path.exists(os.path.join(contest_root, "aichallenge")):
            run_cmd("git clone git://github.com/aichallenge/aichallenge.git")
        else:
            with CD(os.path.join(contest_root, "aichallenge")):
                run_cmd("git pull")
    sub_dir = os.path.join(contest_root, "submissions")
    if not os.path.exists(sub_dir):
        os.mkdir(sub_dir)
        run_cmd("chown contest:contest " + sub_dir)
    map_dir = os.path.join(contest_root, "maps")
    if not os.path.exists(map_dir):
        os.mkdir(map_dir)
        run_cmd("chown contest:contest " + map_dir)
    worker_dir = os.path.join(contest_root, "aichallenge", "worker")
    with CD(worker_dir):
        if not os.path.exists("server_info.py"):
            si_contents = SERVER_INFO_TEMPLATE % (
                os.path.join(contest_root, "aichallenge"), map_dir, sub_dir,
                api_url, api_key)
            with open("server_info.py", "w") as si_file:
                si_file.write(si_contents)
            run_cmd("chmod 600 server_info.py")
            try:
                run_cmd("mysql contest")
            except CmdError:
                run_cmd("echo 'create database worker' | mysql")
                run_cmd("mysql worker < schema.sql")
    run_cmd("chown -R contest:contest /home/contest")

IPTABLES_LOAD = """#!/bin/sh
iptables-restore < /etc/iptables.rules
exit 0
"""

def setup_jailusers(contest_root):
    """ Create and configure the jail users """
    worker_dir = os.path.join(contest_root, "aichallenge", "worker")
    with CD(worker_dir):
        run_cmd("python create_jail_users.py 32")
    org_mode = os.stat("/etc/sudoers")[0]
    os.chmod("/etc/sudoers", 0640)
    append_line("/etc/sudoers", "contest ALL = (%jailusers) NOPASSWD: ALL")
    os.chmod("/etc/sudoers", org_mode)
    run_cmd("iptables-save > /etc/iptables.rules")
    iptablesload_path = "/etc/network/if-pre-up.d/iptablesload"
    if not os.path.exists(iptablesload_path):
        with open(iptablesload_path, "w") as loadfile:
            loadfile.write(IPTABLES_LOAD)
        os.chmod(iptablesload_path, 0744)

def setup_start_script(contest_root, run=False):
    """ Set worker startup script to run on every reboot """
    script = os.path.join(contest_root, "aichallenge/worker/start_worker.sh")
    append_line("/etc/cron.d/ai-games", "@reboot root %s" % (script,))
    if run:
        run_cmd(script)

def full_install(api_key):
    """ Completely set everything up from a fresh ec2 instance """
    with Environ("DEBIAN_FRONTEND", "noninteractive"):
        run_cmd("aptitude update")
        run_cmd("aptitude upgrade -y")
        install_required_packages()
        install_utility_packages()
        install_all_languages()
    setup_contest_user()
    setup_contest_files("/home/contest", "http://ai-contest.com", api_key)
    setup_jailusers("/home/contest")
    setup_start_script("/home/contest", run=True)
    if not file_matches("/root/.bashrc", "^export HISTCONTROL"):
        append_line("/root/.bashrc", "export HISTCONTROL=erasedups")
        append_line("/root/.bashrc", "export HISTSIZE=10000")
        append_line("/root/.bashrc", 'PROMPT_COMMAND="history -a"')
    AUTHKEY_FILE = "/home/ubuntu/.ssh/authorized_keys"
    if (os.path.exists(AUTHKEY_FILE)
            and not file_matches(AUTHKEY_FILE, " contest@ai-contest$")):
        append_line(AUTHKEY_FILE, CONTEST_PUBLIC_KEY)

if __name__ == "__main__":
    api_key = sys.argv[1] if len(sys.argv) > 1 else ""
    full_install(api_key)

