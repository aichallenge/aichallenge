#Archive the starter packs

all: tools.zip

tools.zip: tools/
	zip -r tools.zip tools/

tools/: ../../../ants
	#clean visualizer
	cd ../../visualizer; ant deepclean; cd -
	
	#copy ants folder
	rsync -r ../../* tools/
	
	#clean dist stuff
	rm -Rf tools/website_source/
	mv tools/dist/sample_bots tools/
	rm -Rf tools/dist
	
	#copy engine tools
	cp ../../../worker/sandbox.py tools/
	cp ../../../worker/engine.py tools/

clean:
	-rm -Rf tools/
	-rm tools.zip

install:
	#Copy to website
	mv tools.zip ../../../website/tools.zip

.PHONY: tools all clean
