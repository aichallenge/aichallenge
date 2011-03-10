#!/usr/bin/python
#
# Sets up a contest main server from a fresh ubuntu install.
#
# WARNING: Do NOT run this on a machine that you use for anything else, it
#          currently unconditionally makes very invasive changes the machine
#          configuration
#

import getpass
import os.path
import sys
from optparse import OptionParser

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

def create_contest_user(username):
    """ Create the contest user that all manager scripts run under """
    try:
        run_cmd("getent passwd " + username)
    except CmdError:
        run_cmd('adduser --disabled-password --gecos "" ' + username)

def setup_base_files(opts):
    """ Setup all the contest specific files and directories """
    contest_root = opts.root_dir
    with CD(contest_root):
        if not os.path.exists(os.path.join(contest_root, opts.local_repo)):
            run_cmd("git clone -b %s %s %s" % (opts.src_branch, opts.src_repo,
                opts.local_repo))
        else:
            with CD(os.path.join(contest_root, opts.local_repo)):
                run_cmd("git pull %s %s" % (opts.src_repo, opts.src_branch))
    sub_dir = os.path.join(contest_root, "submissions")
    if not os.path.exists(sub_dir):
        os.mkdir(sub_dir)
        run_cmd("chown {0}:{0} {1}".format(opts.username, sub_dir))
    map_dir = os.path.join(contest_root, "maps")
    if not os.path.exists(map_dir):
        os.mkdir(map_dir)
        run_cmd("chown {0}:{0} {1}".format(opts.username, map_dir))
    si_filename = os.path.join(TEMPLATE_DIR, "server_info.py.template")
    with open(si_filename, 'r') as si_file:
        si_template = si_file.read()
    si_contents = si_template.format(contest_root=contest_root,
            database_user=opts.database_user,
            database_password=opts.database_password,
            database_name=opts.database_name,
            map_dir=map_dir, sub_dir=sub_dir)
    manager_dir = os.path.join(contest_root, opts.local_repo, "manager")
    with CD(manager_dir):
        if not os.path.exists("server_info.py"):
            with open("server_info.py", "w") as si_file:
                si_file.write(si_contents)
            run_cmd("chmod 600 server_info.py")
    run_cmd("chown -R {0}:{0} {1}".format(opts.username, opts.root_dir))

SETUP_SQL = {
    "creation": "create database %s",
    "user_grant_passwd":
            "grant usage on *.* to %s@localhost identified by '%s'",
    "user_grant_nopasswd":
            "grant usage on *.* to %s@localhost",
    "database_perms": "grant all privileges on %s.* to %s@localhost",
    }

def setup_database(opts):
    """ Setup database for contest use """
    import MySQLdb
    try:
        password_opt = ""
        if opts.database_password:
            password_opt = "-p'%s'" % (opts.database_password,)
        run_cmd("echo 'quit' | mysql -u %s %s %s" % (opts.database_user,
            password_opt, opts.database_name))
    except CmdError:
        with MySQLdb.connect(host="127.0.0.1", user="root",
                passwd=opts.database_root_password) as cursor:
            cursor.execute(SETUP_SQL["creation"] % (opts.database_name,))
            if opts.database_user != "root":
                if opts.database_password:
                    cursor.execute(SETUP_SQL["user_grant_passwd"]
                        % (opts.database_user, opts.database_password))
                else:
                    cursor.execute(SETUP_SQL["user_grant_nopasswd"]
                        % (opts.database_user,))
                cursor.execute(SETUP_SQL["database_perms"]
                        % (opts.database_name, opts.database_user))
        # remove from here to comment to switch to new schema layout
        sf = os.path.join(opts.root_dir, opts.local_repo, "sql/000_schema.sql")
        password_opt = ""
        if opts.database_password:
            password_opt = "-p'%s'" % (opts.database_password,)
        run_cmd("mysql -u %s %s %s < %s" % (opts.database_user,
                password_opt, opts.database_name, sf))
        """ This should work with McLeopold's new schema naming system
            in branch epsilon-new-schema
        schema_dir = os.path.join(contest_root, local_repo, "sql")
        schema_files = os.listdir(schema_dir)
        schema_files = [f for f in schema_files if f.endswith(".sql")]
        for sf in schema_files:
            sp = os.path.join(schema_dir, sf)
            run_cmd("mysql contest < " + sp)
        """

def setup_website(opts):
    """ Configure apache to serve the website and set a server_info.php """
    website_root = os.path.join(opts.root_dir, opts.local_repo, "website")
    subs_dir = os.path.join(opts.root_dir, "submissions")
    si_filename = os.path.join(TEMPLATE_DIR, "server_info.php.template")
    with open(si_filename, 'r') as si_file:
        si_template = si_file.read()
    si_contents = si_template.format(sub_dir=subs_dir,
            database_user=opts.database_user,
            database_password=opts.database_password,
            database_name=opts.database_name,
            )
    with CD(website_root):
        if not os.path.exists("server_info.php"):
            with open("server_info.php", "w") as si_file:
                si_file.write(si_contents)
    log_dir = os.path.join(opts.root_dir, "html_logs")
    if not os.path.exists(log_dir):
       os.mkdir(log_dir)
       run_cmd("chown %s:www-data %s" % (opts.username, log_dir))
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
        if opts.website_as_default:
            enabled_link = "/etc/apache2/sites-enabled/000-default"
        else:
            enabled_link = "/etc/apache2/sites-enabled/ai-contest"
        if os.path.exists(enabled_link):
            os.remove(enabled_link)
        os.symlink(site_config, enabled_link)
        run_cmd("/etc/init.d/apache2 restart")

IPTABLES_LOAD = """#!/bin/sh
iptables-restore < /etc/iptables.rules
exit 0
"""

def get_options(argv):
    """ Get all the options required for the installation """
    current_username = os.environ.get("SUDO_USER", getpass.getuser())
    default_install = {
        "installs": set([install_manager_packages, install_website_packages,
            install_basic_languages]),
        "packages_only": False,
        "create_user": False, "username": current_username,
        "database_root_password": "",
        "database_user": current_username, "database_password": "",
        "database_name": "aichallenge", "database_host": "127.0.0.1",
        "root_dir": "/home/$user/ants",
        "src_repo": "git://github.com/aichallenge/aichallenge.git",
        "src_branch": "epsilon",
        "local_repo": "aichallenge",
        "website_as_default": False,
        "website_hostname": "ai-contest.com",
    }
    full_install = {
        "create_user": True, "username": "contest",
        "database_user": "contest", "database_password": "",
        "database_name": "contest",
        "root_dir": "/home/$user",
        "website_as_default": True,
    }

    def replace_options(option, opt_str, value, parser, replacements):
        for option, value in replacements:
            setattr(parser.values, option, value)

    parser = OptionParser()
    parser.set_defaults(**default_install)
    parser.add_option("--full", action="callback", callback=replace_options,
            callback_args = (full_install,),
            help="Do a complete install with layout used for the contest")
    parser.add_option("-r", "--contest_root", action="store", type="string",
            dest="root_dir",
            help="Set the directory where all contest files live")
    options, args = parser.parse_args(argv)
    return options

def main(argv=["server_setup.py"]):
    opts = get_options(argv)
    with Environ("DEBIAN_FRONTEND", "noninteractive"):
        for install in opts.installs:
            install()
    if opts.create_user:
        create_contest_user(opts.username)
    root_dir = opts.root_dir.replace("$user", opts.username)
    if not os.path.exists(root_dir):
        os.makedirs(root_dir)
    opts.root_dir = root_dir
    setup_base_files(opts)
    setup_database(opts)
    setup_website(opts)

if __name__ == "__main__":
    main(sys.argv)

