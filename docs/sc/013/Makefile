# Makefile for SC/13
#
# Relies on environment varialble STARLINK_SGML_DIR being set correctly
#
# $Id$


SGML2DOCS=$(STARLINK_SGML_DIR)/bin/sgml2docs
DOCDIR=/home/norman/s/src/docs

all: sc13.tar-htx sc13.tar-tex sc13-examples.tar

sc13.tar-htx: sc13.sgml ReleaseNotes examples/fpp.c.sgml
	SGML_CATALOG_FILES=$(DOCDIR)/document-summaries/CATALOG:$(STARLINK_SGML_DIR)/dtd/CATALOG \
	BIBINPUTS=$$PWD \
		$(SGML2DOCS) --html --tarball $@ sc13.sgml

sc13.tar-tex: sc13.sgml ReleaseNotes examples/fpp.c.sgml
	SGML_CATALOG_FILES=$(DOCDIR)/document-summaries/CATALOG:$(STARLINK_SGML_DIR)/dtd/CATALOG \
	BIBINPUTS=$$PWD \
		$(SGML2DOCS) --latex --tarball $@ sc13.sgml

# fpp.c.sgml is proxy for the other similar files in examples/
examples/fpp.c.sgml:
	cd examples; $(MAKE) -f Makefile.in sgmlexamples

sc13-examples.tar:
	cd examples; $(MAKE) -f Makefile.in sc13-examples.tar
	ln -s examples/sc13-examples.tar $@
