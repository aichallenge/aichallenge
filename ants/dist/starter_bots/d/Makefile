OUTFILE=MyBot
DMD=dmd
DMD_ARGS=-release -inline -O

SOURCES=$(wildcard *.d)

.PHONY: all unittest clean

all:
	$(DMD) $(DMD_ARGS) -odbin -ofbin/$(OUTFILE) $(SOURCES)
	
unittest:
	$(DMD) $(DMD_ARGS) -odbin -ofbin/$(OUTFILE).unittest -unittest $(SOURCES)
	bin/$(OUTFILE).unittest

clean:
	-rm -Rf bin/*.o bin/$(OUTFILE) bin/$(OUTFILE).unittest

