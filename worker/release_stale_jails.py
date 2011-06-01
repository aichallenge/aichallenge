#!/usr/bin/python
import os
import os.path

CHROOT_PATH = "/srv/chroot"

def main():
    jails = [j for j in os.listdir(CHROOT_PATH) if j.startswith("jailuser")]
    for jail in jails:
        jail_lock = os.path.join(CHROOT_PATH, jail, "locked")
        jail_pid_file = os.path.join(jail_lock, "lock.pid")
        try:
            with open(jail_pid_file) as lock_file:
                lock_pid = int(lock_file.readline())
            print "Found jail %s locked by pid %d." % (jail, lock_pid)
            if not os.path.exists("/proc/%d" % (lock_pid,)):
                print "    jail process does not exist, releasing jail."
                os.unlink(jail_pid_file)
                os.rmdir(jail_lock)
        except IOError as exc:
            if exc.errno != 2:
                raise

if __name__ == "__main__":
    main()

