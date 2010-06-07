# Copyright 2010 owners of the AI Challenge project
#
# Licensed under the Apache License, Version 2.0 (the "License"); you may not
# use this file except in compliance with the License. You may obtain a copy
# of the License at http:#www.apache.org/licenses/LICENSE-2.0 . Unless
# required by applicable law or agreed to in writing, software distributed
# under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
# CONDITIONS OF ANY KIND, either express or implied. See the License for the
# specific language governing permissions and limitations under the License.
#
# Author: Jeff Cameron (jeff@jpcameron.com)
#
# Makefile for the Tic-Tac-Toe contest engine.

TARGETS = Engine.class RenderMap.class

all: $(TARGETS)

clean:
	rm -f *.class keystore *.jar $(TARGETS)

keystore:
	keytool -genkey -alias _alias -keystore keystore -storepass	forthewin -keypass forthewin

visualizer.jar: keystore Viewer.class Manifest.txt
	jar cfvm visualizer.jar Manifest.txt *.class img/*.png img/*.jpg img/*.gif
	jarsigner -keystore keystore -storepass forthewin visualizer.jar _alias

Engine.class: Engine.java Game.class
	javac Engine.java

Fleet.class: Fleet.java
	javac Fleet.java

Game.class: Fleet.class Game.java Planet.class
	javac Game.java

Planet.class: Planet.java
	javac Planet.java

RenderMap.class: RenderMap.java Game.class
	javac RenderMap.java

VizPanel.class: VizPanel.java
	javac VizPanel.java
	
ViewerPanel.class: ViewerPanel.java
	javac ViewerPanel.java

Viewer.class: VizPanel.class ViewerPanel.class Viewer.java Game.class
	javac Viewer.java
	
CLViewer.class: VizPanel.class ViewerPanel.class Viewer.java Game.class
	javac CLViewer.java
