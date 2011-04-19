#!/usr/bin/python

import getpass
import os.path
import re
import sys
from optparse import OptionParser

from install_tools import CD, Environ, install_apt_packages, run_cmd
from install_tools import append_line, file_contains, get_choice, get_password

TEMPLATE_DIR = os.path.dirname(os.path.abspath(__file__))

def install_required_packages():
    """ This installs the packages that are required to run the worker scripts
    """
    pkg_list = ["curl", "unzip"]
    install_apt_packages(pkg_list)

def install_utility_packages():
    """ This installs packages that are nice to have on workers but not
        required for correct operation """
    pkg_list = ["openssh-server", "htop", "screen", "vim"]
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
    RELEASE_TAG = "release.r56"
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

def install_dmd():
    """ Install the D language """
    # FIXME: This is broken because the package below is for i386 only and we
    # are using amd64 workers
    if os.path.exists("/usr/bin/dmd"):
        return
    with CD("/root"):
        run_cmd("curl 'http://ftp.digitalmars.com/dmd_2.052-0_i386.deb' > dmd_2.052-0_i386.deb")
        run_cmd("dpkg -i dmd_2.052-0_i386.deb")

def install_all_languages():
    install_basic_languages()
    install_extra_packaged_languages()
    install_golang()
    install_nodejs()
    install_groovy()
    install_scala()
    # dmd install is broken
    #install_dmd()

def setup_contest_files(opts):
    """ Setup all the contest specific files and directories """
    contest_root = opts.root_dir
    compiled_dir = os.path.join(contest_root, "compiled")
    if not os.path.exists(compiled_dir):
        os.mkdir(compiled_dir)
        run_cmd("chown {0}:{0} {1}".format(opts.username, compiled_dir))
    map_dir = os.path.join(contest_root, "maps")
    if not os.path.exists(map_dir):
        os.mkdir(map_dir)
        run_cmd("chown {0}:{0} {1}".format(opts.username, map_dir))
    worker_dir = os.path.join(contest_root, opts.local_repo, "worker")
    si_filename = os.path.join(TEMPLATE_DIR, "worker_server_info.py.template")
    with open(si_filename, 'r') as si_file:
        si_template = si_file.read()
    si_contents = si_template.format(contest_root=contest_root,
            map_dir=map_dir, compiled_dir=compiled_dir,
            api_url=opts.api_url, api_key=opts.api_key)
    with CD(worker_dir):
        if not os.path.exists("server_info.py"):
            with open("server_info.py", "w") as si_file:
                si_file.write(si_contents)
            run_cmd("chmod 600 server_info.py")
    run_cmd("chown -R {0}:{0} {1}".format(opts.username, contest_root))

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

def setup_start_script(opts):
    """ Set worker startup script to run on every reboot """
    return script

def interactive_options(options):
    print "Warning: This script is meant to be run as root and will make changes to the configuration of the machine it is run on."
    resp = raw_input("Are you sure you want to continue (yes/no)? [no] ")
    sys_update = options.update_system
    options.update_system = get_choice(
            "Update and upgrade system before rest of setup?",
            options.update_system)
    if resp != "yes":
        sys.exit(1)
    pkg_only = get_choice(
            "Only install packages, do no additional setup?")
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
    options.install_cronjobs = get_choice("Install startup cronjob?",
            options.install_cronjobs)
    options.run_worker = get_choice(
            "Immediately run worker start script at end of setup?",
            options.run_worker)

def get_options(argv):
    """ Get all the options required for setup """
    current_username = os.environ.get("SUDO_USER", getpass.getuser())
    top_level = os.path.abspath(os.path.join(TEMPLATE_DIR, ".."))
    root_dir, local_repo = os.path.split(top_level)
    default_setup = {
        "update_system": True,
        "install_utilities": True,
        "install_languages": True,
        "packages_only": False,
        "username": current_username,
        "root_dir": root_dir,
        "local_repo": local_repo,
        "api_url": "http://ai-contest.com/",
        "api_key": "",
        "install_cronjobs": True,
        "run_worker": True,
        "interactive": True,
        }
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
    parser.add_option("--start", action="store_true", dest="run_worker",
            help="Start the worker after finishing setup")
    options, args = parser.parse_args(argv)
    if options.interactive:
        interactive_options(options)
        confirm = raw_input("Run setup as configured above (yes/no)? [no] ")
        if confirm != "yes":
            sys.exit(1)
    return options

def main(argv=["worker_setup.py"]):
    """ Completely set everything up from a fresh ec2 instance """
    opts = get_options(argv)
    with Environ("DEBIAN_FRONTEND", "noninteractive"):
        if opts.update_system:
            run_cmd("aptitude update")
            run_cmd("aptitude upgrade -y")
        install_required_packages()
        if opts.install_utilities:
            install_utility_packages()
        if opts.install_languages:
            install_all_languages()
    if opts.packages_only:
        return
    setup_contest_files(opts)
    #setup_jailusers("/home/contest")
    start_script = os.path.join(opts.root_dir, opts.local_repo,
            "worker/start_worker.sh")
    if opts.install_cronjobs:
        cron_file = "/etc/cron.d/ai-contest"
        if not file_contains(cron_file, start_script):
            append_line(cron_file, "@reboot root %s" % (start_script,))
    if opts.run_worker:
        run_cmd(start_script)

if __name__ == "__main__":
    main(sys.argv)

