# Makefile for sun242 - documentation
#
# Relies on env var STARLINK_SGML_DIR being set correctly.  It is
# therefore not buildable unless you have the SGML kit installed.
#
# $Id$


SGML2DOCS=$(STARLINK_SGML_DIR)/bin/sgml2docs

all: sun242.tar-htx sun242.tar-tex

sun242.tar-htx: sun242.xml #ReleaseNotes
	BIBINPUTS=$$PWD \
		$(SGML2DOCS) --html --tarball=$@ sun242.xml

sun242.tar-tex: sun242.xml #ReleaseNotes
	BIBINPUTS=$$PWD \
		$(SGML2DOCS) --latex --tarball=$@ sun242.xml

# unbuild is a Starlink conventional target, equivalent to tidy here.
unbuild: tidy

tidy:
	rm -f *~
	rm -Rf sun242.htx sun242.latex

clean: tidy
	rm -f sun242.tar-htx sun242.tar-tex
