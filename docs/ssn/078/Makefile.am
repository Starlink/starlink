## Process this file with automake to produce Makefile.in

stardocs_DATA = ssn78.htx_tar ssn78.ps ssn78.pdf

# The following is temporary.  In due course, support for building XML
# documentation will be added to the starconf system, with a
# STAR_XML_DOCUMENTATION to match the existing STAR_LATEX_DOCUMENTATION.
# This depends on the sgmlkit being installed.

# Location of the Jade binary.  OpenJade appears to be installed by default
# on RedHat systems since 7.3.
JADE=@JADE@
SGMLNORM=@SGMLNORM@
METAPOST=@METAPOST@
STARLINK_SGML_DIR=$(prefix)/lib/sgmlkit
SGML2DOCS=$(bindir)/sgml2docs
# pstoimg is a bit creaky, but it should be available, since it's installed
# as part of star2html
PSTOIMG=@PSTOIMG@

## all: ssn78.htx_tar ssn78.ps ssn78.pdf

# Requires that STARLINK_SGML_DIR hold the path to the top of the Starlink
# SGML Kit, so that $STARLINK_SGML_DIR/bin/sgml2docs is the path to the 
# sgml2docs application.  These will work with any version of Jade,
# in particular with openjade.
ssn78.htx_tar: ssn78.xml $(GIFFILES)
	JADE=$(JADE) SGMLNORM=$(SGMLNORM) \
		SGML_CATALOG_FILES=$(STARLINK_SGML_DIR)/dtd/CATALOG-override \
		$(SGML2DOCS) --html --tarball=$@ \
		ssn78.xml
ssn78-single.htx_tar: ssn78.xml $(GIFFILES)
	JADE=$(JADE) SGMLNORM=$(SGMLNORM) \
		SGML_CATALOG_FILES=$(STARLINK_SGML_DIR)/dtd/CATALOG-override \
		$(SGML2DOCS) --htmlsingle --tarball=$@ \
		ssn78.xml

ssn78.texml_tar: ssn78.xml $(EPSFILES) $(PDFFILES)
	JADE=$(JADE) SGMLNORM=$(SGMLNORM) \
		SGML_CATALOG_FILES=$(STARLINK_SGML_DIR)/dtd/CATALOG-override \
		$(SGML2DOCS) --texml --tarball=$@ \
		ssn78.xml

ssn78.ps: ssn78.texml
	cd ssn78.texml; latex ssn78; latex ssn78; dvips -o ssn78.ps ssn78
	cp ssn78.texml/ssn78.ps .
ssn78.pdf: ssn78.texml
	cd ssn78.texml; pdflatex ssn78; pdflatex ssn78
	cp ssn78.texml/ssn78.pdf .
ssn78.texml: ssn78.texml_tar
	tar xf ssn78.texml_tar

# Though the following rule uses a configured pstoimg, it will fail if
# epstopdf doesn't happen to be in the path
SUFFIXES = .eps .pdf .gif
EPSFILES = ssn78-1.eps ssn78-2.eps ssn78-3.eps ssn78-4.eps
PDFFILES = ssn78-1.pdf ssn78-2.pdf ssn78-3.pdf ssn78-4.pdf
GIFFILES = ssn78-1.gif ssn78-2.gif ssn78-3.gif ssn78-4.gif
$(EPSFILES): ssn78.mp
	@METAPOST@ ssn78.mp
	for f in `ls ssn78.* | sed -n 's/.*\.\([0-9]*\)$$/\1/p'`; do \
	    mv ssn78.$$f ssn78-$$f.eps; \
	done
.eps.pdf:
	epstopdf --outfile=$@ $<
.eps.gif:
	L2HMODULE=`dirname $(PSTOIMG)` \
	    $(PSTOIMG) -gif -scale 1.5 -antialias -out $@ $<

#$(PDFFILES): $(EPSFILES)
#for f in $(EPSFILES); do \
#epstopdf --outfile=ssn78-$$f.pdf ssn78-$$f.eps; \
#L2HMODULE=`dirname $(PSTOIMG)` $(PSTOIMG) -gif -scale 1.5 -antialias -out ssn78-$$f.gif ssn78-$$f.eps; \
#done

MOSTLYCLEANFILES = $(EPSFILES) $(PDFFILES) $(GIFFILES)
