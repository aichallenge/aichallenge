JAVAC=javac
JAVAC_ARGS=
#TODO: Apparently this is needed, but I can't find it on my system: -classpath /etc/java-config-2/current-system-vm/jre/lib/plugin.jar

SOURCEPATH = com/aicontest/visualizer/

SOURCES=$(wildcard $(SOURCEPATH)*/)
CLASSES=$(SOURCES:%.java=%.class)

.PHONY: all clean

all: $(CLASSES)

%.class: %.java
	$(JAVAC) $(JAVAC_ARGS) $<

clean:
	-rm -Rf $(SOURCEPATH)*.class