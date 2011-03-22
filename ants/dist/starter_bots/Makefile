#Archive the starter packs

STARTER_PACKS = $(wildcard */)
ARCHIVES = $(STARTER_PACKS:%/=%.zip)

all: archives

archives: $(ARCHIVES)

%.zip: %/
	zip -r $@ $<

clean:
	-rm -Rf $(ARCHIVES)

.PHONY: archives all clean