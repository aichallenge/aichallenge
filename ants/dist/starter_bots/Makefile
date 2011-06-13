#Archive the starter packs

STARTER_BOTS = $(wildcard */)
STARTER_PACKAGES = $(STARTER_BOTS:%/=%_starter_package.zip)

all: starter_packages

starter_packages: $(STARTER_PACKAGES)

%_starter_package.zip: %
	#Make a temp folder to prepare the package
	cp -R $< $<_starter_package

	#Include other things in here ($<_starter_package)

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
