# Makefile for ssn70 - documentation
#
# relies on env var STARLINK_SGML_DIR being set correctly
#
# $Id$


SGML2DOCS=$(STARLINK_SGML_DIR)/bin/sgml2docs

ssn70.tar-htx: ssn70.sgml ReleaseNotes
	cd ../dtd2sgml; $(MAKE) sgmltext.stamp
	cd ../../bin; $(MAKE) sgmlbin.sgml
	cd ../../lib; $(MAKE) sgmllib.sgml
	SGML_CATALOG_FILES=$$PWD/../documents/CATALOG:$(STARLINK_SGML_DIR)/dtd/CATALOG \
	BIBINPUTS=$$PWD \
		$(SGML2DOCS) --html --tarball $@ ssn70.sgml

ssn70.tar-tex: ssn70.sgml ReleaseNotes
	cd ../dtd2sgml; $(MAKE) sgmltext.stamp
	cd ../../bin; $(MAKE) sgmlbin.sgml
	cd ../../lib; $(MAKE) sgmllib.sgml
	SGML_CATALOG_FILES=$$PWD/../documents/CATALOG:$(STARLINK_SGML_DIR)/dtd/CATALOG \
	BIBINPUTS=$$PWD \
		$(SGML2DOCS) --latex --tarball $@ ssn70.sgml

#../dtd2sgml/sgmltext.stamp:
#	cd ../dtd2sgml; $(MAKE) sgmltext.stamp
