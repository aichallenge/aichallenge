SUBDIRS = examples

all: CppStarterPackage.zip JavaStarterPackage.zip

clean:
	rm -rf CppStarterPackage JavaStarterPackage *.zip tools example_bots maps
	for dir in $(SUBDIRS); \
	do \
		$(MAKE) -C $$dir clean; \
	done

CppStarterPackage.zip: example_bots maps tools
	mkdir CppStarterPackage
	cp cpp/MyBot.cc CppStarterPackage
	cp cpp/PlanetWars.cc CppStarterPackage
	cp cpp/PlanetWars.h CppStarterPackage
	cp cpp/Makefile CppStarterPackage
	cp -r tools CppStarterPackage/
	cp -r example_bots CppStarterPackage/
	cp -r maps CppStarterPackage/
	zip -r CppStarterPackage.zip CppStarterPackage

example_bots: examples
	mkdir example_bots
	cp examples/*.jar example_bots
	cp examples/*.java example_bots

JavaStarterPackage.zip: example_bots maps tools
	mkdir JavaStarterPackage
	cp java/MyBot.java JavaStarterPackage
	cp java/PlanetWars.java JavaStarterPackage
	cp java/Fleet.java JavaStarterPackage
	cp java/Planet.java JavaStarterPackage
	cp -r tools JavaStarterPackage/
	cp -r example_bots JavaStarterPackage/
	cp -r maps JavaStarterPackage/
	zip -r JavaStarterPackage.zip JavaStarterPackage

maps:
	mkdir maps
	cp ../maps/*.txt maps

.PHONY: subdirs $(SUBDIRS)
subdirs: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@

tools:
	mkdir tools
	cp ../viz/PlayGame.jar tools
	cp ../viz/ShowGame.jar tools
