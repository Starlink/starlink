# Expresses the dependencies of sgmllib.sgml on its input files, and
# manages creating them from the script sources.
#
# $Id$

SCRIPTS=img-eqlist.pl

libdocs.sgml: $(SCRIPTS)
	cp libdocs.head $@
	for f in $(SCRIPTS); do sed -f script2sgml.sed $$f >> $@; done

