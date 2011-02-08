AI Challenge Worker
===================

*Codename: Zeta*

This module hosts logic to run the worker nodes for the AI Challenge.
It is currently under heavy development.

Each submodule is responsible for a specific aspect of the worker's operation.

Submodules
----------

* `worker.config` - Loading configuration options for the worker from file
* `worker.games` - Submodules each contain logic for running a particular game
    * `worker.games.higher_number` - Sample game for demonstrative purposes
* `worker.language` - Programming language support and compiling submissions
* `worker.log` - Local logging services, primarily for debugging purposes
* `worker.process` - Worker startup routine and main loop
* `worker.runner` - Running bots in a constrained environment and enforcing time limits
* `worker.storage` - Data retrieval and decompression from centralized storage
* `worker.submission` - Local model which maintains submission status information

Environment
-----------

Parts of the worker (most notably the runner) expect a certain setup. This will be
streamlined in the future (i.e. packaged as a DEB and integrated into an AMI);
until that happens (and perhaps still afterwards), it is documented here.

There should be an `aichallenge` user, under which the main worker process will run.
There should also be some number of sequentially numbered runner user accounts:
`aichallenge-run0` and so on. Currently, a maximum of 100 are supported, but you need
only create as many as you expect to user. Be advised that the runner does maintain a
lock on a user account while running, so there should be at least as many as you expect
simultaneous bots to be running. These runner users should all be members of the
`aichallenge-run` group.

In order to give the `aichallenge` user access to run bots and send signals to them even
after forfeiting root privilege, a sudoers line is required:

    aichallenge ALL=(%aichallenge-run) NOPASSWD: ALL

In the most recent release of Ubuntu Server, this should be stored in
`/etc/sudoers.d/aichallenge`, owned by root:root with permissions 0440. If this is not
possible, the line can be added to the main sudoers file. The `aichallenge` user should
not have other access to use sudo.

The contents of the repository should exist in `/usr/lib/aichallenge`. This directory
need not be writable other than by root, though during development it may be convenient
to have it writable to your user account. No data files are stored here.

For convenience, it is recommended that a symbolic link from `/usr/bin/aichallenge` to
`/usr/lib/aichallenge/aichallenge.py` be created. This enables the use of the command-line
`aichallenge` utility.

The configuration file (sample available in `aichallenge.conf.sample`) should be stored
in `/etc/aichallenge.conf`, and only needs to be readable by root as the worker reads its
configuration before switching UIDs. Critically, it should *not* be readable to the runner
users as this would allow private credentials and configuration options to become exposed.

A submission cache is maintained in `/var/lib/aichallenge/submissions`. This directory
should exist. The following owner/group/permissions are recommended:

    drwx--s--- 4 aichallenge aichallenge-run  /var/lib/aichallenge/submissions/

This permission mode ensures that the runners have access to the cached submission only if
they are accessed by their SHA-1 hash (that is, it is not possible for bots to iterate over
the submission cache),

During execution, the worker will create lock files corresponding to the runner users in
`/var/run/aichallenge`. Logs are stored in `/var/log/aichallenge`, which should exist and
be writable by the `aichallenge` user.

If the environment variable AICHALLENGE_PREFIX exists upon invocation, all of these paths will
be relative to that root instead of /.

To start a worker, run:

    sudo aichallenge worker start

Multiple worker instances can be started by adding the -n parameter and supplying the desired
number. To stop all workers, kill all runner processes and clean up lock files, run:

    sudo aichallenge worker stop

Init scripts will likely be created which call these at some point in the future.

Currently, to satisfy the Python library dependencies, the worker requires the following APT
packages:

* `python-boto` - for Amazon Web Services support
* `python-argparse` - for parsing command-line arguments
* `python-simplejson` - for parsing and constructing JSON messages
* `python-setproctitle` - for setting the process title
