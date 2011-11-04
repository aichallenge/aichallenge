FPC=fpc
FLAGS=-Mdelphi
SOURCES=*.pas
RELEASE_FLAGS=-Si -O3 -Xs -FErelease
DEBUG_FLAGS=-Sa -vewni -O1 -CioRrt -bl -glv -FEdebug
RELEASE_OBJECTS=$($(basename ${SOURCES}):%=release/%.o) $($(basename ${SOURCES}):%=release/%.ppu)
DEBUG_OBJECTS=$($(basename ${SOURCES}):%=debug/%.o) $($(basename ${SOURCES}):%=debug/%.ppu)
RELEASE_BIN=release/MyBot
DEBUG_BIN=debug/MyBot

.PHONY: all debug clean

all: $(DEBUG_BIN) $(RELEASE_BIN)

debug: $(DEBUG_BIN)

release: $(RELEASE_BIN)

$(DEBUG_BIN): $(SOURCES)
	mkdir -p debug
	$(FPC) $(FLAGS) $(DEBUG_FLAGS) MyBot.pas

$(RELEASE_BIN): $(SOURCES)
	mkdir -p release
	$(FPC) $(FLAGS) $(RELEASE_FLAGS) MyBot.pas

clean: 
	-rm -rf release debug
