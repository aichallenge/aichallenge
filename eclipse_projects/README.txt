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
plugin      : JavaScript Development Tools
              http://download.eclipse.org/releases/indigo/

project     : Java wrapper for the visualizer
plugin      : Eclipse Java Development Tools
              http://download.eclipse.org/releases/indigo/
note        : The user library 'Java plugin' must be created and list the JAR
              file that contains the Java Netscape plugin code.
              (jre/lib/plugin.jar for Sun/Oracle VMs)

project     : Python
plugin      : PyDev for Eclipse
              http://pydev.org/updates
note        : Set up the interpreter.

project     : website
plugin      : PHP Development Tools (PDT) Runtime Feature
              http://download.eclipse.org/releases/indigo/


HINTS
-----

- It is probably best to create a workspace outside the working copy and import
  the projects there.
- Shared launchers are *.launch files in the project. Right-click them and use
  the Run As -> ... command to run and register them with your workspace.
