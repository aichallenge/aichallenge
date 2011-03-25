OUTFILE=MyBot
DMD=dmd
DMD_ARGS=-release -inline -O

SOURCES=$(wildcard *.d)

.PHONY: all clean

all:
	$(DMD) $(DMD_ARGS) -of$(OUTFILE) $(SOURCES)
	
unittest:
	$(DMD) $(DMD_ARGS) -of$(OUTFILE).unittest -unittest $(SOURCES)
	$(OUTFILE).unittest

clean:
	-rm -Rf *.o $(OUTFILE) $(OUTFILE).unittest
