# Archive the starter packs

export ANT_OPTS := -Xmx100m

all: tools.zip

tools.zip: tools/
	zip -r tools.zip tools/ -x '*.sh'
	tar -caf tools.tar.bz2 tools --exclude '*.cmd'

tools/: ../../../ants
	# copy clean source tree of 'ants' over here
	cd ../..; git checkout-index -a --prefix=ants/dist/tools/
	mv ants tools
	
	# remove git typical entries
	find tools -type f -name .gitignore     -delete
	find tools -type f -name .gitattributes -delete
	
	# clean dist stuff and visualizer
	mv tools/dist/sample_bots tools/
	rm -Rf tools/dist
	# util/block_test.sh references a starter bot, if we keep this in,
	# the sed line below must be uncommented
	rm -Rf tools/util
	rm -Rf tools/website-source
	rm -Rf tools/visualizer/

	# copy engine tools
	cp ../../../worker/sandbox.py tools/
	cp ../../../worker/engine.py tools/
	
	# remove "dist/" or "dist\" from paths in .cmd and .sh
	sed -i 's/dist[/\]//g' tools/play_one_game*
	#sed -i 's/dist[/\]//g' tools/util/block_test.*

	# We compile the java visualizer from the original directory, 
	# reusing already downloaded external files, but use our new copy
	# as the destination directory. (Keep in mind any options you may
	# have overridden in visualizer/local.properties)
	cd ../../visualizer; ant jar html-visualizer
	cp ../../visualizer/build/deploy/visualizer.jar tools/
	unzip ../../visualizer/build/html_visualizer.zip -d tools/visualizer
	
	#Include VERSION file
	echo "This tools package was published on `date`." >> tools/VERSION
	echo >> tools/VERSION
	
	echo "Last change in the ants/ folder was:" >> tools/VERSION
	git log -n 1 ../../|grep -v Author >> tools/VERSION
	echo >> tools/VERSION
	
	echo "Check https://github.com/aichallenge/aichallenge/ants/ for new versions." >> tools/VERSION

clean:
	-rm -Rf tools/
	-rm -f tools.*

install:
	#Copy to website
	cp tools.* ../../../website/

.PHONY: tools all clean
