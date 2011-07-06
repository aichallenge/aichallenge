# Archive the starter packs

export ANT_OPTS := -Xmx100m

all: tools.zip

tools.zip: ants/
	zip -r tools.zip ants/ -x '*.sh'
	tar -caf tools.tar.lzma ants --exclude '*.cmd'

ants/: ../../../ants
	# copy clean source tree of 'ants' over here
	cd ../..; git checkout-index -a --prefix=ants/dist/tools/
	
	# remove git typical entries
	find ants -type f -name .gitignore     -delete
	find ants -type f -name .gitattributes -delete
	
	# clean dist stuff and visualizer
	mv ants/dist/sample_bots ants/
	rm -Rf ants/dist
	# util/block_test.sh references a starter bot, if we keep this in,
	# the sed line below must be uncommented
	rm -Rf ants/util
	rm -Rf ants/website-source
	rm -Rf ants/visualizer/

	# copy engine tools
	cp ../../../worker/sandbox.py ants/
	cp ../../../worker/engine.py ants/
	
	# remove "dist/" or "dist\" from paths in .cmd and .sh
	sed -i 's/dist[/\]//g' ants/play_one_game.*
	#sed -i 's/dist[/\]//g' ants/util/block_test.*

	# We compile the java visualizer from the original directory, 
	# reusing already downloaded external files, but use our new copy
	# as the destination directory. (Keep in mind any options you may
	# have overridden in visualizer/local.properties)
	cd ../../visualizer; ant jar html-visualizer
	cp ../../visualizer/build/deploy/visualizer.jar ants/
	unzip ../../visualizer/build/deploy/html_visualizer.zip -d ants/visualizer

clean:
	-rm -Rf ants/
	-rm tools.zip

install:
	#Copy to website
	cp tools.* ../../../website/

.PHONY: tools all clean
