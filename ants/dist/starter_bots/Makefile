#Archive the starter packs

STARTER_BOTS = $(wildcard */)
STARTER_PACKAGES = $(STARTER_BOTS:%/=%_starter_package.zip)

all: starter_packages

starter_packages: $(STARTER_PACKAGES)

%_starter_package.zip: %
	#Make a temp folder to prepare the package
	cp -R $< $<_starter_package
	
	#Include other things in here ($<_starter_package)
	
	#Include VERSION file
	echo "This starter package zip was published on `date`." >> $<_starter_package/VERSION
	echo >> $<_starter_package/VERSION
	
	echo "Last change on this starter package was:" >> $<_starter_package/VERSION
	git log -n 1 $<|grep -v Author >> $<_starter_package/VERSION
	echo >> $<_starter_package/VERSION
	
	echo "Check https://github.com/aichallenge/aichallenge/tree/epsilon/ants/dist/starter_bots/"$<" for new versions." >> $<_starter_package/VERSION
	
	#Zip that folder
	cd $<_starter_package; zip -r ../$@ *; cd -
	
	#Remove temp folder
	rm -Rf $<_starter_package

clean:
	#zips
	-rm -Rf $(STARTER_PACKAGES)
	
	#temp folders
	-rm -Rf *_starter_package/
	
	#installed website folders
	-rm -Rf ../../../website/starter_packages/*.zip

install:
	#Copy to website
	mv *.zip ../../../website/starter_packages/

.PHONY: archives all clean
