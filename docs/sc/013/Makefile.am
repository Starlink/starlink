## Process this file with automake to produce Makefile.in

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@
starexamples_DATA = sc13.$(VERSION)-examples.tar.gz

sc13.$(VERSION)-examples.tar.gz:
	cd examples; $(MAKE) dist
	ln -f examples/sc13-examples-1.0.tar.gz $@
