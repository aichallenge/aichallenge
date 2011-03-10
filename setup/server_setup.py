#!/usr/bin/python
#
# Sets up a contest main server from a fresh ubuntu install.
#
# WARNING: Do NOT run this on a machine that you use for anything else, it
#          currently unconditionally makes very invasive changes the machine
#          configuration
#

import os.path
import sys

from install_tools import CD, Environ, install_apt_packages, run_cmd, CmdError

TEMPLATE_DIR = os.path.dirname(os.path.abspath(__file__))

def install_manager_packages():
    """ Install basic system packages required for the manager """
    pkg_list = ["mysql-server", "python-mysqldb", "openssh-server", "make",
            "git", "cron", "unzip", "python-simplejson"]
    install_apt_packages(pkg_list)

def install_website_packages():
    """ Install system packages required for the website """
    pkg_list = ["apache2", "php5", "libapache2-mod-php5", "php5-mysql"]
    install_apt_packages(pkg_list)

def install_basic_languages():
    """ Install base set of submission languages,
        currently C, C++, Java and Python """
    pkg_list = ["gcc", "g++", "openjdk-6-jdk", "python-dev"]
    install_apt_packages(pkg_list)

def setup_contest_user():
    """ Setup the contest user that all manager scripts run under """
    if not os.path.exists("/home/contest"):
        run_cmd('adduser --disabled-password --gecos "" contest')

def setup_contest_files(contest_root, src_repo, local_repo, branch="epsilon"):
    """ Setup all the contest specific files and directories """
    with CD(contest_root):
        if not os.path.exists(os.path.join(contest_root, local_repo)):
            run_cmd("git clone -b %s %s %s" % (branch, src_repo, local_repo))
        else:
            with CD(os.path.join(contest_root, local_repo)):
                run_cmd("git pull %s %s" % (src_repo, branch))
    sub_dir = os.path.join(contest_root, "submissions")
    if not os.path.exists(sub_dir):
        os.mkdir(sub_dir)
        run_cmd("chown contest:contest " + sub_dir)
    map_dir = os.path.join(contest_root, "maps")
    if not os.path.exists(map_dir):
        os.mkdir(map_dir)
        run_cmd("chown contest:contest " + map_dir)
    si_filename = os.path.join(TEMPLATE_DIR, "server_info.py.template")
    with open(si_filename, 'r') as si_file:
        si_template = si_file.read()
    si_contents = si_template.format(contest_root=contest_root,
            map_dir=map_dir, sub_dir=sub_dir)
    manager_dir = os.path.join(contest_root, local_repo, "manager")
    with CD(manager_dir):
        if not os.path.exists("server_info.py"):
            with open("server_info.py", "w") as si_file:
                si_file.write(si_contents)
            run_cmd("chmod 600 server_info.py")
        try:
            run_cmd("echo 'quit' | mysql contest")
        except CmdError:
            run_cmd("echo 'create database contest' | mysql")
            # remove from here to comment to switch to new schema layout
            sf = os.path.join(contest_root, local_repo,
                    "sql/000_schema.sql")
            run_cmd("mysql contest < " + sf)
            """ This works with McLeopold's new schema naming system
                in branch epsilon-new-schema
            schema_dir = os.path.join(contest_root, local_repo, "sql")
            schema_files = os.listdir(schema_dir)
            schema_files = [f for f in schema_files if f.endswith(".sql")]
            for sf in schema_files:
                sp = os.path.join(schema_dir, sf)
                run_cmd("mysql contest < " + sp)
            """
    website_root = os.path.join(contest_root, local_repo, "website")
    configure_website(contest_root, website_root, sub_dir)
    run_cmd("chown -R contest:contest /home/contest")

def configure_website(contest_root, website_root, sub_dir):
    """ Configure apache to serve the website and set a server_info.php """
    si_filename = os.path.join(TEMPLATE_DIR, "server_info.php.template")
    with open(si_filename, 'r') as si_file:
        si_template = si_file.read()
    si_contents = si_template.format(sub_dir=sub_dir)
    with CD(website_root):
        if not os.path.exists("server_info.php"):
            with open("server_info.php", "w") as si_file:
                si_file.write(si_contents)
    log_dir = os.path.join(contest_root, "html_logs")
    if not os.path.exists(log_dir):
       os.mkdir(log_dir)
       run_cmd("chown contest:www-data " + log_dir)
       os.chmod(log_dir, 0775)
    site_config = "/etc/apache2/sites-available/ai-contest"
    if not os.path.exists(site_config):
        site_filename = os.path.join(TEMPLATE_DIR, "apache_site.template")
        with open(site_filename, "r") as site_file:
            site_template = site_file.read()
        site_contents = site_template.format(web_hostname="ai-contest.com",
                web_root=website_root, log_dir=log_dir)
        with open(site_config, "w") as site_file:
            site_file.write(site_contents)
        enabled_link = "/etc/apache2/sites-enabled/000-default"
        if os.path.exists(enabled_link):
            os.remove(enabled_link)
        os.symlink(site_config, enabled_link)
        run_cmd("/etc/init.d/apache2 restart")

IPTABLES_LOAD = """#!/bin/sh
iptables-restore < /etc/iptables.rules
exit 0
"""

def main():
    with Environ("DEBIAN_FRONTEND", "noninteractive"):
        install_manager_packages()
        install_website_packages()
        install_basic_languages()
    setup_contest_user()
    SRC_REPO = "git://github.com/aichallenge/aichallenge.git"
    setup_contest_files("/home/contest", SRC_REPO, "aichallenge")

if __name__ == "__main__":
    main()

