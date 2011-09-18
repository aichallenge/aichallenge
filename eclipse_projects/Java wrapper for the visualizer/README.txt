1. JAVA PLUGIN PATH IN ANT BUILD
--------------------------------
If you don't use the Sun/Oracle JRE or don't have the JAVA_HOME environment
variable, you need to setup the path to the Java plugin for the ant build
script. Create a file named "local.properties" next to "default.properties" and
set the "java.plugin" variable. The following is an example location for 
IcedTea 6 on Gentoo Linux:

java.plugin: /opt/icedtea6-web-bin/share/icedtea-web/plugin.jar

2. DOWNLOAD ADDITIONAL FILES
----------------------------
1. Select the Java perspective (Window -> Open Perspective -> Java).
2. Display the Ant view (Window -> Show View -> Ant).
3. Add "visualizer/build.xml" to the view via drag&drop or by clicking on the
   add button.
4. Double-click the "jar" ant tasks to run it. This will include downloads
   of files that are listed as missing in Eclipse and build the visualizer.jar.
5. Right-click on the project and select refresh. (You can configure the ant
   launcher, that was newly created, to do this automatically in the future.)

3. JAVA PLUGIN PATH IN ECLIPSE
------------------------------
To keep the project 'clean' I added a place holder library named 'Java plugin'
that has to be filled with the actual location of the Java plugin jar.

1. Right-click on the project and select "Properties".
2. Select "Java Build Path" on the left and then the "Libraries" tab.
3. Click on "Java plugin" and then the "Edit..." button.
4. Create a User library named "Java plugin" that contains the Java plugin for
   your Java Runtime.
   (e.g.: <...>/jre/lib/plugin.jar or that path from you local.properties file)

All displayed errors should now be gone.