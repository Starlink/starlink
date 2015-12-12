## Process this file with automake to produce Makefile.in

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@
starexamples_DATA = sc13.$(VERSION)-examples.tar.gz

sc13.htx_tar: examples/fpp.c.xml
sc13.texml_tar: examples/fpp.c.xml

# fpp.c.xml is proxy for the other similar files in examples/
examples/fpp.c.xml:
	cd examples; test -f configure || sh ./bootstrap
	cd examples; ./configure
	cd examples; $(MAKE) xmlexamples
# make sure it's up-to-date -- above doesn't always do it
	touch $@

sc13.$(VERSION)-examples.tar.gz:
	cd examples; $(MAKE) dist
	ln -f examples/sc13-examples-1.0.tar.gz $@
