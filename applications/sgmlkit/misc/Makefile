# Expresses the dependencies of {lib,bin}docs.sgml on their input files, and
# manages creating them from the script sources.
#
# $Id$

LIBSCRIPTS=../lib/img-eqlist.pl ../lib/abstract-star2html.pl
BINSCRIPTS=../bin/sgml2docs ../bin/sgml2xml ../bin/sgml2hlp

misc.stamp: libdocs.sgml bindocs.sgml
	touch $@

libdocs.sgml: $(LIBSCRIPTS)
	cp libdocs.head $@
	for f in $(LIBSCRIPTS); do sed -f script2sgml.sed $$f >> $@; done

bindocs.sgml: $(BINSCRIPTS)
	cp bindocs.head $@
	for f in $(BINSCRIPTS); do sed -f script2sgml.sed $$f >> $@; done

maintainer-clean distclean: clean
clean:
	rm -f misc.stamp libdocs.sgml bindocs.sgml
