INFO
----

This is a place that you can use as an Eclipse workspace and import projects
into. The projects work in Eclipse 3.7 (Indigo) with various language plugins.
The "Platform Runtime Binary" is enough to get started, but any of the bundles
will do as well. Start Eclipse and set the workspace to "ants/eclipse_workspace" 
inside your working copy of the git repository. Then install the plugins and
import projects according to the table below.
Most Eclipse plugins offer detailed code analysis and formatting options that
help in writing clean code, so it is worth to check for invalid characters in
html or missing tags in PHP with it.
I'm not generally a fan of putting IDE specific files up on source repositories,
but I think with the wide language support, Eclipse offers a good platform for
our project.

- Marco 'mleise' Leise


PROJECTS
--------

project     : JavaScript visualizer
import from : ants/eclipse_workspace
plugin      : JavaScript Development Tools
              http://download.eclipse.org/releases/indigo/

project     : Java wrapper for the visualizer
import from : ants/eclipse_workspace
plugin      : Eclipse Java Development Tools
              http://download.eclipse.org/releases/indigo/
note        : The user library 'Java plugin' must be created and list the JAR
              file that contains the Java Netscape plugin code.
              (jre/lib/plugin.jar for Sun/Oracle VMs)

project     : Python
import from : ants/eclipse_workspace
plugin      : PyDev for Eclipse
              http://pydev.org/updates
note        : Set up the interpreter.

project     : website
import from : ants/eclipse_workspace
plugin      : PHP Development Tools (PDT) Runtime Feature
              http://download.eclipse.org/releases/indigo/

project     : D starter bot
import from : ants/dist/starter_bots
plugin      : D Development Tools
              http://ddt.eclipselabs.org.codespot.com/hg.updates/
note        : Set up the interpreter.


HINTS
-----

- The ant file (build.xml) for the visualizer can be used in the Java
  perspective from the 'Ant' view. This is a side bar that lists all targets in
  an ant script and executes them on double-click.
- Eclipse workspaces aren't portable (yet), so the projects have to be imported
  manually. (They are referenced by absolute paths.)
