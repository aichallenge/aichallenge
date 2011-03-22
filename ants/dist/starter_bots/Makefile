#Archive the starter packs

STARTER_BOTS = $(wildcard */)
STARTER_PACKAGES = $(STARTER_BOTS:%/=%_starter_package.zip)

all: starter_packages

starter_packages: $(STARTER_PACKAGES)

%_starter_package.zip: %
	#Make a temp folder to prepare the package
	cp -R $< $<_starter_package

	#Include other things in here ($<_starter_package)
	if [ "$<" = "common_lisp" ]; then (cd $<_starter_package; bin/download-and-install-3rd-party-libs.sh); fi

	#Zip that folder
	zip -r $@ $<_starter_package

	#Remove temp folder
	rm -Rf $<_starter_package

clean:
	#zips
	-rm -Rf $(STARTER_PACKAGES)

	#temp folders
	-rm -Rf *_starter_package/

.PHONY: archives all clean