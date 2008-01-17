## Process this file with automake to produce Makefile.in

%.gz: %
	rm -f $@
	gzip --best --force $<

dvi2bitmap-docs.tar: ssn71.xml ReleaseNotes ../VERSION
	test "$(JADE)" != NOJADE -a "$(SGMLNORM)" != NOSGMLNORM && \
	  JADE=$(JADE) SGMLNORM=$(SGMLNORM) \
	    SGML_CATALOG_FILES=$(SGMLKIT_HOME)/dtd/CATALOG-override \
	    $(SGML2DOCS) --html --tarball=$@ $<

ssn71.texml_tar: ssn71.xml ReleaseNotes ../VERSION
	test "$(JADE)" != NOJADE -a "$(SGMLNORM)" != NOSGMLNORM && \
	  JADE=$(JADE) SGMLNORM=$(SGMLNORM) \
	    SGML_CATALOG_FILES=$(SGMLKIT_HOME)/dtd/CATALOG-override \
	    $(SGML2DOCS) --texml --tarball=$@ $<

dvi2bitmap-docs.pdf: ssn71.texml_tar
	tar xf ssn71.texml_tar
	cd ssn71.texml && pdflatex ssn71 && pdflatex ssn71
	mv ssn71.texml/ssn71.pdf ./dvi2bitmap-docs.pdf
