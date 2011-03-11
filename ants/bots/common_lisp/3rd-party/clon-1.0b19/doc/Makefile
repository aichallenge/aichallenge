### Makefile --- Doc directory

## Copyright (C) 2010, 2011 Didier Verna

## Author:        Didier Verna <didier@lrde.epita.fr>
## Maintainer:    Didier Verna <didier@lrde.epita.fr>

## This file is part of Clon.

## Permission to use, copy, modify, and distribute this software for any
## purpose with or without fee is hereby granted, provided that the above
## copyright notice and this permission notice appear in all copies.

## THIS SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
## WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
## MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
## ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
## WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
## ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
## OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.


### Commentary:

## Contents management by FCM version 0.1.


### Code:

TOP_DIR := ..

include $(TOP_DIR)/Makefile.cnf

hack: all

include $(TOP_DIR)/Makefile.inc
include $(TOP_DIR)/version.inc


INFO_DIR     := $(PREFIX)/share/info
DOC_DIR	     := $(SHARE)/doc
CLON_DOC_DIR := $(DOC_DIR)/clon
TEXI_FILES   := user.texi enduser.texi

INFO_FILES     := $(TEXI_FILES:%.texi=clon-%.info)
PDF_FILES      := $(TEXI_FILES:%.texi=%.pdf)
ALL_INFO_FILES := $(INFO_FILES:%.info=%.info*)
DVI_FILES      := $(TEXI_FILES:%.texi=%.dvi)
PS_FILES       := $(TEXI_FILES:%.texi=%.ps)
HTML_DIRS      := $(TEXI_FILES:%.texi=%)


all: info pdf html

ref: info-ref pdf-ref html-ref

webref: pdf-webref html-webref

all-formats: all dvi ps

all-formats-ref: ref dvi-ref ps-ref

install-at: $(PDF_FILES) $(HTML_DIRS)
	install -d "$(LOCATION)"
	install -m 644 $(PDF_FILES) "$(LOCATION)/"
	install -d -m 755 "$(LOCATION)/user"
	install -m 644 user/* "$(LOCATION)/user/"
	install -d -m 755 "$(LOCATION)/enduser"
	install -m 644 enduser/* "$(LOCATION)/enduser/"

install: $(INFO_FILES)
	install -d "$(INFO_DIR)"
	install -m 644 $(ALL_INFO_FILES) "$(INFO_DIR)/"
	for i in $(INFO_FILES); do			\
	  install-info --info-dir="$(INFO_DIR)" $$i;	\
	done
	$(MAKE) install-at LOCATION="$(CLON_DOC_DIR)"
	-for i in $(DVI_FILES) $(PS_FILES); do			\
	  test -f $$i && install -m 644 $$i "$(CLON_DOC_DIR)";	\
	done
	-if test -f clon-reference.info; then				\
	  install -m 644 clon-reference.info* "$(INFO_DIR)/" ;		\
	  install-info --info-dir="$(INFO_DIR)" clon-reference.info ;	\
	fi
	-if test -f reference.pdf ; then				\
	  install -m 644 reference.pdf "$(CLON_DOC_DIR)/" ;	\
	fi
	-if test -f reference ; then					\
	  install -d -m 755 "$(CLON_DOC_DIR)/reference" ;		\
	  install -m 644 reference/* "$(CLON_DOC_DIR)/reference/" ;	\
	fi
	-for i in reference.dvi reference.ps; do			\
	  test -f $$i && install -m 644 $$i "$(CLON_DOC_DIR)";	\
	done

uninstall:
	-rm -fr "$(CLON_DOC_DIR)"
	for i in $(INFO_FILES) clon-reference.info; do		\
	  install-info --remove --info-dir="$(INFO_DIR)" $$i;	\
	done
	cd "$(INFO_DIR)" && rm -f $(ALL_INFO_FILES) clon-reference.info*

info: $(INFO_FILES)
info-ref: clon-reference.info

pdf: clon.jpg $(PDF_FILES)
pdf-ref: clon.jpg reference.pdf
pdf-webref: clon.jpg webreference.pdf

html: $(HTML_DIRS)
html-ref: reference
html-webref: webreference

dvi: clon.eps $(DVI_FILES)
dvi-ref: clon.eps reference.dvi

ps: clon.eps $(PS_FILES)
ps-ref: clon.eps reference.ps

reference.texi: generate.cl
	$(SBCL_PATH) $(SBCL_LOAD) ./generate.cl
	$(PERL) -pi -e 's/^\@author/\@vskip 2cm\n\@image\{clon,16cm\}\n\@author/' $@

webreference.texi: generate.cl
	$(SBCL_PATH) $(SBCL_LOAD) ./generate.cl --web
	$(PERL) -pi -e 's/^\@author/\@vskip 2cm\n\@image\{clon,16cm\}\n\@author/' $@

clon-%.info: %.texi
	$(MAKEINFO) $<

%.pdf: %.texi
	$(TEXI2DVI) -p $<

$(HTML_DIRS) reference webreference: %: %.texi
	$(MAKEINFO) --html -o $@ $<
	touch $@

%.dvi: %.texi
	$(TEXI2DVI) $<

%.ps: %.dvi
	$(DVIPS) -o $@ $<

%.eps: %.jpg
	$(CONVERT) $< $@

clean:
	-rm *.tmp *.fn* *.vr* *.ky* *.tp* *.cp* *.pg *.toc *.aux *.log *~

distclean: clean
	-rm -fr clon.eps webreference* clon-reference* reference*	\
		$(HTML_DIRS)						\
		$(ALL_INFO_FILES) $(PDF_FILES) $(DVI_FILES) $(PS_FILES)

update-version:
	for f in $(TEXI_FILES); do					   \
	  $(PERL) -pi -e 's/@set VERSION .*/@set VERSION $(LONG_VERSION)/' \
	    $$f;							   \
	done

install-www: webref
	$(MAKE) install-at LOCATION="$(W3DIR)"
	install -m 644 webreference.pdf "$(W3DIR)/reference.pdf"
	install -d -m 755 "$(W3DIR)/reference"
	install -m 644 webreference/* "$(W3DIR)/reference/"


.PHONY: all ref webref all-formats all-formats-ref	\
	install-at install uninstall			\
	info info-ref					\
	pdf pdf-ref pdf-webref				\
	html html-ref html-webref			\
	dvi dvi-ref					\
	ps ps-ref					\
	clean distclean					\
	update-version					\
	install-www


### Makefile ends here
