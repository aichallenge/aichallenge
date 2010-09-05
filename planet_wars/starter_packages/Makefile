SUBDIRS = examples
TARGETS = cpp_starter_package.zip csharp_starter_package.zip java_starter_package.zip python_starter_package.zip

all: $(TARGETS)

clean:
	rm -rf *_starter_package $(TARGETS) tools example_bots maps
	for dir in $(SUBDIRS); \
	do \
		$(MAKE) -C $$dir clean; \
	done

cpp_starter_package.zip: example_bots maps tools
	mkdir cpp_starter_package
	cp cpp/MyBot.cc cpp_starter_package
	cp cpp/PlanetWars.cc cpp_starter_package
	cp cpp/PlanetWars.h cpp_starter_package
	cp cpp/Makefile cpp_starter_package
	cp -r tools cpp_starter_package/
	cp -r example_bots cpp_starter_package/
	cp -r maps cpp_starter_package/
	cp common/README.txt cpp_starter_package/
	zip -r cpp_starter_package.zip cpp_starter_package

csharp_starter_package.zip: example_bots maps tools
	mkdir csharp_starter_package
	cp csharp/MyBot.cs csharp_starter_package
	cp csharp/PlanetWars.cs csharp_starter_package
	cp csharp/Planet.cs csharp_starter_package
	cp csharp/Fleet.cs csharp_starter_package
	cp -r tools csharp_starter_package/
	cp -r example_bots csharp_starter_package/
	cp -r maps csharp_starter_package/
	cp common/README.txt csharp_starter_package/
	zip -r csharp_starter_package.zip csharp_starter_package

example_bots: examples
	mkdir example_bots
	cp examples/*.jar example_bots
	cp examples/*.java example_bots

java_starter_package.zip: example_bots maps tools
	mkdir java_starter_package
	cp java/MyBot.java java_starter_package
	cp java/PlanetWars.java java_starter_package
	cp java/Fleet.java java_starter_package
	cp java/Planet.java java_starter_package
	cp -r tools java_starter_package/
	cp -r example_bots java_starter_package/
	cp -r maps java_starter_package/
	cp common/README.txt java_starter_package/
	zip -r java_starter_package.zip java_starter_package

maps:
	mkdir maps
	cp ../maps/*.txt maps

python_starter_package.zip: example_bots maps tools
	mkdir python_starter_package
	cp python/MyBot.py python_starter_package
	cp python/PlanetWars.py python_starter_package
	cp -r tools python_starter_package/
	cp -r example_bots python_starter_package/
	cp -r maps python_starter_package/
	cp common/README.txt python_starter_package/
	zip -r python_starter_package.zip python_starter_package

.PHONY: subdirs $(SUBDIRS)
subdirs: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@

tools:
	mkdir tools
	cp ../viz/PlayGame.jar tools
	cp ../viz/ShowGame.jar tools
