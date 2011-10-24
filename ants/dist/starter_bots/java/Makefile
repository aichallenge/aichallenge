JAVAC=javac
JAVAC_ARGS=

SOURCES=Ants.java Bot.java Aim.java Ilk.java Order.java Tile.java AbstractSystemInputParser.java AbstractSystemInputReader.java MyBot.java
CLASSES=$(SOURCES:%.java=%.class)
JAR=MyBot.jar

.PHONY: all clean

all: $(CLASSES) $(JAR)

$(JAR): $(CLASSES)
	jar cvfm $(JAR) Manifest.txt $(CLASSES)

%.class: %.java
	$(JAVAC) $(JAVAC_ARGS) $<

clean:
	-rm -Rf $(CLASSES)
	-rm -Rf *.class
	-rm -Rf $(JAR)