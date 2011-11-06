CC=gcc
CFLAGS=-O3 -funroll-loops -c
LDFLAGS=-O2
LDLIBS=-lm
SOURCES=MyBot.c YourCode.c ants.c
HEADERS=ants.h
OBJECTS=$(addsuffix .o, $(basename ${SOURCES}))
EXECUTABLE=MyBot

all: $(EXECUTABLE)

$(EXECUTABLE): $(OBJECTS)

$(OBJECTS): %.o: %.c $(HEADERS)

clean:
	rm -f $(EXECUTABLE) $(OBJECTS)

.PHONY: all clean
