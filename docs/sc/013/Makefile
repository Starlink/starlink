# Makefile for SC/13
#
# Relies on environment varialble STARLINK_SGML_DIR being set correctly
#
# $Id$


SGML2DOCS=$(STARLINK_SGML_DIR)/bin/sgml2docs
DOCDIR=/home/norman/s/src/docs
VERSION=$(shell cat VERSION)

%.gz: %
	gzip -9 -c $< >$@

all: sc13.$(VERSION)-package.tar

sc13.$(VERSION)-package.tar: \
		sc13.$(VERSION).tar-htx.gz sc13.$(VERSION).tar-tex.gz \
		sc13.$(VERSION)-examples.tar.gz sc13.$(VERSION).ps.gz
	tar cf $@ sc13.$(VERSION).tar-htx.gz sc13.$(VERSION).tar-tex.gz \
		sc13.$(VERSION)-examples.tar.gz sc13.$(VERSION).ps.gz

sc13.$(VERSION).tar-htx: sc13.sgml ReleaseNotes examples/fpp.c.sgml
	SGML_CATALOG_FILES=$(DOCDIR)/document-summaries/CATALOG:$(STARLINK_SGML_DIR)/dtd/CATALOG \
	BIBINPUTS=$$PWD \
		$(SGML2DOCS) --html --tarball $@ sc13.sgml

sc13.$(VERSION).tar-tex: sc13.sgml ReleaseNotes examples/fpp.c.sgml
	SGML_CATALOG_FILES=$(DOCDIR)/document-summaries/CATALOG:$(STARLINK_SGML_DIR)/dtd/CATALOG \
	BIBINPUTS=$$PWD \
		$(SGML2DOCS) --latex --tarball $@ sc13.sgml

# fpp.c.sgml is proxy for the other similar files in examples/
examples/fpp.c.sgml:
	cd examples; $(MAKE) -f Makefile.in sgmlexamples

sc13.$(VERSION).ps: sc13.$(VERSION).tar-tex
	tar xf sc13.$(VERSION).tar-tex
	cd sc13.latex; \
		latex sc13; \
		latex sc13; \
		dvips sc13 -o sc13.ps
	ln sc13.latex/sc13.ps $@

sc13.$(VERSION)-examples.tar:
	cd examples; $(MAKE) -f Makefile.in sc13-examples.tar
	ln examples/sc13-examples.tar $@
