#Archive the starter packs

all: tools.zip

tools.zip: tools/
	zip -r tools.zip tools/

tools/: ../../../ants
	rsync -r ../../../ants/* tools/
	
	rm -Rf tools/dist/starter_bots/
	rm -Rf tools/dist/tools/
	rm -Rf tools/website_source/
	
	cp ../../../worker/sandbox.py tools/
	cp ../../../worker/engine.py tools/

clean:
	-rm -Rf tools/
	-rm tools.zip

install:
	#Copy to website
	mv *.zip ../../../website/starter_packages/

.PHONY: tools all clean
