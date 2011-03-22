CCZIP=compiler-latest.zip
CCDOwNLOAD=http://closure-compiler.googlecode.com/files/$(CCZIP)
RUNJAR=java -jar
GREP=grep
SED=sed
CP=cp
WGET=wget
UNZIP=unzip -o
MKDIRS=mkdir -p

.PHONY: all download compile

all: download compile

download:
	if [ ! -d ClosureCompiler ]; then $(MKDIRS) ClosureCompiler && cd ClosureCompiler && $(WGET) $(CCDOwNLOAD) && $(UNZIP) $(CCZIP) && $(RM) $(CCZIP) && cd ..; fi

compile:
	$(RUNJAR) ClosureCompiler/compiler.jar $$($(GREP) "^\$$import(" visualizer.js | $(SED) "s/^\$$import('/--js=/;s/');/.js/") --js_output_file visualizer-min.js --compilation_level ADVANCED_OPTIMIZATIONS

clean:
	$(RM) visualizer-min.js

install:
	mkdir -p $(DESTDIR)/visualizer/js
	$(CP) visualizer-min.js $(DESTDIR)/visualizer/js/visualizer.js

