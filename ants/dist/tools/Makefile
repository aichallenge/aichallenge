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
	
	#remove "dist/" or "dist\" from paths
	sed -i 's/dist[/\]//g' tools/util/block_test.cmd tools/util/block_test.sh tools/play_one_game.cmd tools/play_one_game.sh

clean:
	-rm -Rf tools/
	-rm tools.zip

install:
	#Copy to website
	mv tools.zip ../../../website/tools.zip

.PHONY: tools all clean
