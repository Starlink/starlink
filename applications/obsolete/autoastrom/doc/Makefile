# Makefile for sun242 - documentation
#
# Relies on env var STARLINK_SGML_DIR being set correctly.  It is
# therefore not buildable unless you have the SGML kit installed.
#
# $Id$

LATEX=pdflatex
SGML2DOCS=$(STARLINK_SGML_DIR)/bin/sgml2docs

all: sun242.tar-htx sun242.tar-texml

sun242.tar-htx: sun242.xml #ReleaseNotes
	BIBINPUTS=$$PWD \
		$(SGML2DOCS) --html --tarball=$@ sun242.xml

sun242.tar-htxsingle: sun242.xml #ReleaseNotes
	BIBINPUTS=$$PWD \
		$(SGML2DOCS) --htmlsingle --packagedir sun242.htxsingle \
		--tarball=$@ sun242.xml

sun242.tar-texml: sun242.xml
	BIBINPUTS=$$PWD \
	  $(SGML2DOCS) --texml --tarball=$@ sun242.xml

sun242.pdf: sun242.tar-texml
	rm -Rf $@ sun242.texml
	tar xf sun242.tar-texml
	cd sun242.texml; \
	  $(LATEX) sun242; \
	  if grep 'Rerun to get cross-references' sun242.log >/dev/null; then \
	    $(LATEX) sun242; \
	  fi
	ln sun242.texml/sun242.pdf ./sun242.pdf

# unbuild is a Starlink conventional target, equivalent to tidy here.
unbuild: tidy

tidy:
	rm -f *~
	rm -Rf sun242.htx sun242.latex

clean: tidy
	rm -f sun242.tar-htx sun242.tar-texml sun242.pdf
	rm -Rf sun242.texml
