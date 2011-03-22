JAVAC=javac
JAVAC_ARGS=

SOURCES=HunterBot.java LeftyBot.java RandomBot.java
CLASSES=$(SOURCES:%.java=%.class)

.PHONY: all clean

all: $(CLASSES)

%.class: %.java
	$(JAVAC) $(JAVAC_ARGS) $<

clean:
	-rm -Rf *.class