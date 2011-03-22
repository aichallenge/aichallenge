JAVAC=javac
JAVAC_ARGS=-classpath $(JAVA_HOME)/jre/lib/plugin.jar
JAR=jar
FIND=find
CP=cp

SOURCES=$(wildcard com/aicontest/visualizer/*.java)
CLASSES=$(SOURCES:%.java=%.class)

.PHONY: all compile clean

all: compile jar

compile: $(JAVA_SOURCES)
	$(JAVAC) $(JAVAC_ARGS) `$(FIND) . -name '*.java'`

jar:
	mkdir -p ../applet
	$(JAR) cf ../applet/canvas.jar `$(FIND) . -name '*.class'`

clean:
	$(FIND) . -name '*.class' -delete
	$(RM) ../applet/canvas.jar

install:
	mkdir -p $(DESTDIR)/visualizer/applet
	$(CP) ../applet/canvas.jar $(DESTDIR)/visualizer/applet

