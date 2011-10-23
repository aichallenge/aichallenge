JAVAC=javac
JAVAC_ARGS=

SOURCES=MyBot.java
CLASSES=MyBot.class Ants.class Bot.class Aim.class Ilk.class Order.class Tile.class AbstractSystemInputParser.class AbstractSystemInputReader.class

.PHONY: all clean

all: $(CLASSES)
	jar cvfm MyBot.jar Manifest.txt $(CLASSES)

%.class: %.java
	$(JAVAC) $(JAVAC_ARGS) $<

clean:
	-rm -Rf *.class
	-rm -Rf *.jar