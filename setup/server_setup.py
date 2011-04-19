#!/usr/bin/python
#
# Sets up a contest main server.
#
#

import getpass
import os.path
import sys
from optparse import OptionParser

import create_worker_archive
from install_tools import CD, Environ, install_apt_packages, run_cmd, CmdError
from install_tools import get_choice, get_password

TEMPLATE_DIR = os.path.dirname(os.path.abspath(__file__))

def install_manager_packages():
    """ Install basic system packages required for the manager """
    pkg_list = ["mysql-server", "python-mysqldb", "openssh-server", "make",
            "git", "cron", "unzip"]
    install_apt_packages(pkg_list)

def install_website_packages():
    """ Install system packages required for the website """
    pkg_list = ["apache2", "php5", "libapache2-mod-php5", "php5-mysql"]
    install_apt_packages(pkg_list)

def setup_base_files(opts):
    """ Setup all the contest specific files and directories """
    sub_dir = os.path.join(opts.root_dir, "submissions")
    if not os.path.exists(sub_dir):
        os.mkdir(sub_dir)
        run_cmd("chown {0}:{0} {1}".format(opts.username, sub_dir))
    map_dir = os.path.join(opts.root_dir, "maps")
    if not os.path.exists(map_dir):
        os.mkdir(map_dir)
        run_cmd("chown {0}:{0} {1}".format(opts.username, map_dir))
    si_filename = os.path.join(TEMPLATE_DIR, "server_info.py.template")
    with open(si_filename, 'r') as si_file:
        si_template = si_file.read()
    si_contents = si_template.format(contest_root=opts.root_dir,
            database_user=opts.database_user,
            database_password=opts.database_password,
            database_name=opts.database_name,
            map_dir=map_dir, sub_dir=sub_dir)
    manager_dir = os.path.join(opts.root_dir, opts.local_repo, "manager")
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
        password_opt = ""
        if opts.database_password:
            password_opt = "-p'%s'" % (opts.database_password,)
        schema_dir = os.path.join(opts.root_dir, opts.local_repo, "sql")
        schema_files = os.listdir(schema_dir)
        schema_files = [f for f in schema_files if f.endswith(".sql")]
        schema_files.sort()
        for sf in schema_files:
            sp = os.path.join(schema_dir, sf)
            run_cmd("mysql -u %s %s %s < %s" % (opts.database_user,
                password_opt, opts.database_name, sp))

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
    if not os.path.exists(os.path.join(website_root, "aicontest.tgz")):
        create_worker_archive.main(website_root)
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

def interactive_options(options):
    print "Warning: This script is meant to be run as root and will make changes to the configuration of the machine it is run on."
    resp = raw_input("Are you sure you want to continue (yes/no)? [no] ")
    if resp != "yes":
        sys.exit(1)
    pkg_only = get_choice("Only install system packages?")
    options.packages_only = pkg_only
    if pkg_only:
        print "Only system packages will be installed, no further setup will be done."
        return
    user = options.username
    user = raw_input("Contest username? [%s] " % (user,))
    options.username = user if user else options.username
    options.database_root_password = get_password("database root")
    db_user = options.database_user
    db_user = raw_input("Contest database username? [%s] " % (db_user,))
    options.database_user = db_user if db_user else options.database_user
    options.database_password = get_password("contest database")
    db_name = options.database_name
    db_name = raw_input("Name of contest database? [%s] " % (db_name,))
    options.database_name = db_name if db_name else options.database_name
    root_dir = options.root_dir
    root_dir = raw_input("Contest root directory? [%s] " % (root_dir,))
    options.root_dir = root_dir if root_dir else options.root_dir
    repo_dir = options.local_repo
    repo_dir = raw_input("Directory of source repository? [%s] " % (repo_dir,))
    options.local_repo = repo_dir if repo_dir else options.local_repo
    webname = options.website_hostname
    webname = raw_input("Website hostname? [%s] " % (webname,))
    options.website_hostname = webname if webname else options.website_hostname
    default = options.website_as_default
    default = get_choice("Make contest site the default website?", default)
    options.website_as_default = default

def get_options(argv):
    """ Get all the options required for the installation """
    current_username = os.environ.get("SUDO_USER", getpass.getuser())
    # find default paths
    top_level = os.path.abspath(os.path.join(TEMPLATE_DIR, ".."))
    root_dir, local_repo = os.path.split(top_level)
    default_install = {
        "installs": set([install_manager_packages, install_website_packages]),
        "packages_only": False,
        "username": current_username,
        "database_root_password": "",
        "database_user": current_username, "database_password": "",
        "database_name": "aichallenge",
        "root_dir": root_dir,
        "local_repo": local_repo,
        "website_as_default": False,
        "website_hostname": "ai-contest.com",
        "interactive": True,
    }
    full_install = {
        "website_as_default": True,
        "interactive": False,
    }

    def replace_options(option, opt_str, value, parser, replacements):
        for option, value in replacements.items():
            setattr(parser.values, option, value)

    parser = OptionParser()
    parser.set_defaults(**default_install)
    parser.add_option("--take-over-server", action="callback",
            callback=replace_options, callback_args = (full_install,),
            help="Do a complete install as used for the contest")
    parser.add_option("-r", "--contest_root", action="store", type="string",
            dest="root_dir",
            help="Set the directory where all contest files live")
    parser.add_option("--packages-only", action="store_true",
            dest="packages_only",
            help="Only install required packages and exit")
    parser.add_option("-u", "--username", action="store", dest="username",
            help="User account for the contest")
    options, args = parser.parse_args(argv)
    if options.interactive:
        interactive_options(options)
        confirm = raw_input("Run setup as configured above (yes/no)? [no] ")
        if confirm != "yes":
            sys.exit(1)
    return options

def main(argv=["server_setup.py"]):
    opts = get_options(argv)
    with Environ("DEBIAN_FRONTEND", "noninteractive"):
        for install in opts.installs:
            install()
    if opts.packages_only:
        return
    setup_base_files(opts)
    setup_database(opts)
    setup_website(opts)

if __name__ == "__main__":
    main(sys.argv)

