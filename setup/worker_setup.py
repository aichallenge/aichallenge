#!/usr/bin/python

import grp
import getpass
import os
import os.path
import pwd
import re
import sys
from optparse import OptionParser, SUPPRESS_HELP

from install_tools import CD, Environ, install_apt_packages, run_cmd
from install_tools import append_line, file_contains, get_choice, get_password, check_ubuntu_version
from socket import getfqdn

TEMPLATE_DIR = os.path.dirname(os.path.abspath(__file__))

def install_required_packages():
    """ This installs the packages that are required to run the worker scripts
    """
    pkg_list = ["curl", "unzip", "python-software-properties"]
    install_apt_packages(pkg_list)

def install_utility_packages():
    """ This installs packages that are nice to have on workers but not
        required for correct operation """
    pkg_list = ["openssh-server", "htop", "screen", "vim"]
    install_apt_packages(pkg_list)

def install_basic_languages():
    """ Install base set of submission languages,
        currently C, C++, Java and Python """
    pkg_list = ["gcc", "g++", "openjdk-6-jdk", "python-dev", "python3-dev"]
    install_apt_packages(pkg_list)

def install_extra_distribution_languages():
    """ Install all extra languages that are part of the Ubuntu distribution
        and don't require any special installation steps """
    pkg_list = ["ruby1.9.1", "php5-cli", "perl", "ocaml", "luajit", "ghc",
            "common-lisp-controller", "sbcl", "mono-2.0-devel"]
    install_apt_packages(pkg_list)
    if not os.path.exists("/usr/bin/ruby"):
        os.symlink("/usr/bin/ruby1.9.1", "/usr/bin/ruby")

def install_golang():
    """ Install golang """
    run_cmd("add-apt-repository ppa:gophers/go")
    run_cmd("apt-get update")
    install_apt_packages(['golang'])

def install_nodejs():
    """ Install node.js """
    run_cmd("add-apt-repository ppa:jerome-etienne/neoip")
    run_cmd("apt-get update")
    install_apt_packages(['nodejs'])

def install_coffeescript():
    """ Install coffeescript """
    if os.path.exists("/usr/local/bin/coffee"):
        return
    run_cmd("curl http://npmjs.org/install.sh | clean=no sh")
    run_cmd("npm install -g coffee-script")

def install_clojure():
    """ Install the Clojure language """
    if os.path.exists("/usr/share/java/clojure.jar"):
        return
    with CD("/root"):
        run_cmd("curl 'http://cloud.github.com/downloads/clojure/clojure/clojure-1.2.0.zip' > clojure-1.2.0.zip")
        run_cmd("unzip clojure-1.2.0.zip")
        run_cmd("cp clojure-1.2.0/clojure.jar /usr/share/java")

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
        run_cmd("curl 'http://www.scala-lang.org/downloads/distrib/files/scala-2.9.0.1.tgz' | tar xz")
        os.rename("scala-2.9.0.1", "/usr/share/scala")
        os.symlink("/usr/share/scala/bin/scala", "/usr/bin/scala")
        os.symlink("/usr/share/scala/bin/scalac", "/usr/bin/scalac")

def install_dmd():
    """ Install the D language """
    if os.path.exists("/usr/bin/dmd"):
        return
    install_apt_packages("gcc-multilib")
    with CD("/root"):
        run_cmd("curl 'http://ftp.digitalmars.com/dmd_2.053-0_amd64.deb' > dmd_2.053-0_amd64.deb")
        run_cmd("dpkg -i dmd_2.053-0_amd64.deb")

def install_packaged_languages():
    install_basic_languages()
    install_extra_distribution_languages()
    install_golang()
    install_nodejs()

def install_all_languages():
    install_basic_languages()
    install_extra_distribution_languages()
    install_golang()
    install_nodejs()
    install_coffeescript()
    install_clojure()
    install_groovy()
    install_scala()
    install_dmd()

def install_jailguard(options):
    worker_dir = os.path.join(options.local_repo, "worker")
    run_cmd("cp %s/jailguard.py /usr/local/bin/" % (worker_dir,))

def setup_contest_files(options):
    """ Setup all the contest specific files and directories """
    contest_root = options.root_dir
    local_repo = options.local_repo
    compiled_dir = os.path.join(contest_root, "compiled")
    if not os.path.exists(compiled_dir):
        os.mkdir(compiled_dir)
        run_cmd("chown {0}: {1}".format(options.username, compiled_dir))
    map_dir = os.path.join(contest_root, "maps")
    if not os.path.exists(map_dir):
        os.mkdir(map_dir)
        run_cmd("chown {0}: {1}".format(options.username, map_dir))
    if not os.path.exists(options.log_dir):
        os.mkdir(options.log_dir)
        run_cmd("chown {0}: {1}".format(options.username, options.log_dir))
    worker_dir = os.path.join(contest_root, local_repo, "worker")
    si_filename = os.path.join(TEMPLATE_DIR, "worker_server_info.py.template")
    with open(si_filename, 'r') as si_file:
        si_template = si_file.read()
    si_contents = si_template.format(contest_root=contest_root,
            repo_dir=local_repo, log_dir=options.log_dir,
            map_dir=map_dir, compiled_dir=compiled_dir,
            api_url=options.api_url, api_key=options.api_key)
    with CD(worker_dir):
        if not os.path.exists("server_info.py"):
            with open("server_info.py", "w") as si_file:
                si_file.write(si_contents)
            run_cmd("chown {0}:{0} server_info.py".format(options.username))
    if os.stat(local_repo).st_uid != pwd.getpwnam(options.username).pw_uid:
        run_cmd("chown -R {0}: {1}".format(options.username, local_repo))

def setup_base_chroot(options):
    """ Create and setup the base chroot jail users will run in. """
    install_apt_packages(["debootstrap", "schroot", "unionfs-fuse", "gcc"])
    chroot_dir = "/srv/chroot"
    base_chroot_dir = os.path.join(chroot_dir, "aic-base")
    if not os.path.exists(base_chroot_dir):
        os.makedirs(base_chroot_dir)
        run_cmd("debootstrap --variant=buildd --arch %s natty \
                %s http://us.archive.ubuntu.com/ubuntu/" % (options.arch, base_chroot_dir,))
        with CD(TEMPLATE_DIR):
            run_cmd("cp chroot_configs/chroot.d/aic-base /etc/schroot/chroot.d/")
            run_cmd("cp chroot_configs/sources.list %s/etc/apt/"
                    % (base_chroot_dir,))
            run_cmd("cp -r chroot_configs/ai-jail /etc/schroot/ai-jail")
        deb_archives = "/var/cache/apt/archives/"
        run_cmd("cp {0}*.deb {1}{0}".format(deb_archives, base_chroot_dir))
        run_cmd("schroot -c aic-base -- /bin/sh -c \"\
                DEBIANFRONTEND=noninteractive;\
                apt-get update; apt-get upgrade -y\"")
        run_cmd("schroot -c aic-base -- apt-get install -y python")
    run_cmd("schroot -c aic-base -- %s/setup/worker_setup.py --chroot-base"
            % (os.path.join(options.root_dir, options.local_repo),))

def create_jail_group(options):
    """ Create user group for jail users and set limits on it """
    if not file_contains("/etc/group", "^jailusers"):
        run_cmd("groupadd jailusers")
        run_cmd("groupadd jailkeeper")
        run_cmd("usermod -a -G jailkeeper %s" % (options.username,))
    limits_conf = "/etc/security/limits.conf"
    if not file_contains(limits_conf, "@jailusers"):
        # limit jailuser processes to:
        # 10 processes or system threads
        append_line(limits_conf, "@jailusers hard nproc 20 # ai-contest")
        # 20 minutes of cpu time
        append_line(limits_conf, "@jailusers hard cpu 35 # ai-contest")
        # slightly more than 1GB of ram
        append_line(limits_conf, "@jailusers hard rss 1048600 # ai-contest")
    if not file_contains("/etc/sudoers",
            "^%s.+jailusers" % (options.username,)):
        org_mode = os.stat("/etc/sudoers")[0]
        os.chmod("/etc/sudoers", 0640)
        append_line("/etc/sudoers",
                "%s ALL = (%%jailusers) NOPASSWD: ALL" % (options.username,))
        os.chmod("/etc/sudoers", org_mode)

def create_jail_user(username):
    """ Setup a jail user with the given username """
    run_cmd("useradd -g jailusers -d /home/jailuser %s" % (username,))
    # Add rule to drop any network communication from this user
    run_cmd("iptables -A OUTPUT -m owner --uid-owner %s -j DROP" % (username,))
    # Create user specific chroot
    chroot_dir = "/srv/chroot"
    jail_dir = os.path.join(chroot_dir, username)
    os.makedirs(os.path.join(jail_dir, "scratch"))
    os.makedirs(os.path.join(jail_dir, "root"))
    home_dir = os.path.join(jail_dir, "home/home/jailuser")
    os.makedirs(home_dir)
    run_cmd("chown %s:jailusers %s" % (username, home_dir))
    run_cmd("chown :jailkeeper %s" % (jail_dir,))
    run_cmd("chmod g=rwx %s" % (jail_dir,))
    fs_line = "unionfs-fuse#%s=rw:%s=ro:%s=ro %s fuse cow,allow_other 0 0" % (
            os.path.join(jail_dir, "scratch"),
            os.path.join(jail_dir, "home"),
            os.path.join(chroot_dir, "aic-base"),
            os.path.join(jail_dir, "root")
            )
    append_line("/etc/fstab", fs_line)
    run_cmd("mount %s" % (os.path.join(jail_dir, "root"),))
    cfg_filename = os.path.join(TEMPLATE_DIR,
        "chroot_configs/chroot.d/jailuser.template")
    with open(cfg_filename, 'r') as cfg_file:
        cfg = cfg_file.read()
    schroot_filename = os.path.join("/etc/schroot/chroot.d", username)
    with open(schroot_filename, 'w') as schroot_file:
        schroot_file.write(cfg.format(jailname=username))

IPTABLES_LOAD = """#!/bin/sh
iptables-restore < /etc/iptables.rules
exit 0
"""

def setup_base_jail(options):
    """ Create and configure base jail """
    run_cmd("schroot -c aic-base -- %s/setup/worker_setup.py --chroot-setup"
            % (os.path.join(options.root_dir, options.local_repo),))
    create_jail_group(options)
    iptablesload_path = "/etc/network/if-pre-up.d/iptablesload"
    if not os.path.exists(iptablesload_path):
        with open(iptablesload_path, "w") as loadfile:
            loadfile.write(IPTABLES_LOAD)
        os.chmod(iptablesload_path, 0744)
    worker_dir = os.path.join(options.root_dir, options.local_repo, "worker")
    with CD(worker_dir):
        user_info = pwd.getpwnam(options.username)
        cuid = user_info.pw_uid
        cgid = user_info.pw_gid
        jgid = grp.getgrnam("jailusers").gr_gid
        run_cmd("gcc -DCONTEST_UID=%d -DCONTEST_GID=%d -DJAIL_GID=%d jail_own.c -o jail_own" % (cuid, cgid, jgid))
        run_cmd("chown root:%s jail_own" % (cgid,))
        run_cmd("chmod u=rwxs,g=rwx,o= jail_own")

def setup_jailusers(options):
    """ Create and configure the jail users """
    for user_num in range(1, 33):
        create_jail_user("jailuser%s" % (user_num,))
    run_cmd("iptables-save > /etc/iptables.rules")

def interactive_options(options):
    print "Warning: This script is meant to be run as root and will make changes to the configuration of the machine it is run on."
    resp = raw_input("Are you sure you want to continue (yes/no)? [no] ")
    if resp != "yes":
        sys.exit(1)
    sys_update = options.update_system
    options.update_system = get_choice(
            "Update and upgrade system before rest of setup?",
            options.update_system)
    options.create_jails = get_choice("Create bot jails?", options.create_jails)
    pkg_only = get_choice(
            "Only install packages and base jail, do no additional setup?",
            options.packages_only)
    options.packages_only = pkg_only
    if pkg_only:
        print "Only system packages will be installed, no further setup will be done."
        return
    user = options.username
    user = raw_input("Contest username? [%s] " % (user,))
    options.username = user if user else options.username
    root_dir = options.root_dir
    root_dir = raw_input("Contest root directory? [%s] " % (root_dir,))
    options.root_dir = root_dir if root_dir else options.root_dir
    repo_dir = options.local_repo
    repo_dir = raw_input("Directory of source repository? [%s] " % (repo_dir,))
    options.local_repo = repo_dir if repo_dir else options.local_repo
    base_url = options.api_url
    base_url = raw_input("API Base url? [%s] " % (base_url,))
    options.api_url = base_url if base_url else options.api_url
    api_key = options.api_key
    api_key = raw_input("Worker API key? [%s] " % (api_key,))
    options.api_key = api_key if api_key else options.api_key
    options.install_cronjob = get_choice("Install startup cronjob?",
            options.install_cronjob)
    options.run_worker = get_choice(
            "Immediately run worker start script at end of setup?",
            options.run_worker)

def get_options(argv):
    """ Get all the options required for setup """
    current_username = os.environ.get("SUDO_USER", getpass.getuser())
    top_level = os.path.abspath(os.path.join(TEMPLATE_DIR, ".."))
    root_dir = os.path.split(top_level)[0]
    map_dir = os.path.join(root_dir, 'maps')
    replay_dir = os.path.join(root_dir, 'games')
    upload_dir = os.path.join(root_dir, 'uploads')
    compiled_dir = os.path.join(root_dir, 'compiled')
    log_dir = os.path.join(root_dir, 'logs')
    default_setup = {
        "update_system": True,
        "install_required": True,
        "install_utilities": True,
        "install_languages": False,
        "install_pkg_languages": False,
        "install_jailguard": False,
        "packages_only": False,
        "username": current_username,
        "root_dir": root_dir,
        "map_dir": map_dir,
        "replay_dir": replay_dir,
        "upload_dir": upload_dir,
        "compiled_dir": compiled_dir,
        "log_dir": log_dir,
        "local_repo": top_level,
        "create_jails": True,
        "api_url":  "http://"+ '.'.join(getfqdn().split('.')[1:]) +"/",
        "api_key": "",
        "install_cronjob": False,
        "run_worker": False,
        "interactive": True,
        }

    chroot_base = {
        "install_utilities": False,
        "install_pkg_languages": True,
        "install_languages": False,
        "install_jailguard": False,
        "create_jails": False,
        "packages_only": True,
        "interactive": False,
        }
    chroot_setup = {
        "install_utilities": False,
        "install_languages": True,
        "install_jailguard": True,
        "create_jails": False,
        "packages_only": True,
        "interactive": False,
        }
    def replace_options(option, opt_str, opt_value, parser, replacements):
        for option, value in replacements.items():
            setattr(parser.values, option, value)

    parser = OptionParser()
    parser.set_defaults(**default_setup)
    parser.add_option("-y", "--non-interactive", action="store_false",
            dest="interactive",
            help="Run script with no interactive configuration")
    parser.add_option("-r", "--contest_root", action="store", type="string",
            dest="root_dir",
            help="Set the directory where all contest files live")
    parser.add_option("--packages-only", action="store_true",
            dest="packages_only",
            help="Only install required packages and exit")
    parser.add_option("-u", "--username", action="store", dest="username",
            help="User account for the contest")
    parser.add_option("-k", "--api-key", action="store", dest="api_key",
            help="Api key used when communicating with the main server")
    parser.add_option("-b", "--api-url", action="store", dest="api_url",
            help="Base url for queries to the main server")
    parser.add_option("--install-cronjob", action="store_true",
            dest="install_cronjob",
            help="Install cron script to start worker running after reboot")
    parser.add_option("--start", action="store_true", dest="run_worker",
            help="Start the worker after finishing setup")
    parser.add_option("--chroot-setup", action="callback",
            callback=replace_options, callback_args=(chroot_setup,),
            help=SUPPRESS_HELP)
    parser.add_option("--chroot-base", action="callback",
            callback=replace_options, callback_args=(chroot_base,),
            help=SUPPRESS_HELP)
    options, args = parser.parse_args(argv)
    if options.interactive:
        interactive_options(options)
        confirm = raw_input("Run setup as configured above (yes/no)? [no] ")
        if confirm != "yes":
            sys.exit(1)
    return options

def main(argv=["worker_setup.py"]):
    """ Completely set everything up from a fresh ec2 instance """
    _, ubuntu_arch = check_ubuntu_version()
    opts = get_options(argv)
    opts.arch = ubuntu_arch
    with Environ("DEBIAN_FRONTEND", "noninteractive"):
        if opts.update_system:
            run_cmd("apt-get update")
            run_cmd("apt-get upgrade -y")
        if opts.install_required:
            install_required_packages()
        if opts.install_utilities:
            install_utility_packages()
        if opts.install_pkg_languages:
            install_packaged_languages()
        if opts.install_languages:
            install_all_languages()
    if opts.install_jailguard:
        install_jailguard(opts)
    if opts.create_jails:
        setup_base_chroot(opts)
    if opts.packages_only:
        return
    setup_contest_files(opts)
    if opts.create_jails:
        setup_base_jail(opts)
        setup_jailusers(opts)
    start_script = os.path.join(opts.root_dir, opts.local_repo,
            "worker/start_worker.sh")
    if opts.install_cronjob:
        cron_file = "/etc/cron.d/ai-contest"
        if not file_contains(cron_file, start_script):
            append_line(cron_file, "@reboot %s %s"
                    % (opts.username, start_script,))
    if opts.run_worker:
        run_cmd("sudo -u %s %s" % (opts.username, start_script))

if __name__ == "__main__":
    main(sys.argv)
