# Expresses the dependencies of bindocs.sgml on its input files, and
# manages creating them from the script sources.
#
# $Id$

SCRIPTS=sgml2docs sgml2xml sgml2hlp

bindocs.sgml: $(SCRIPTS)
	cp bindocs.head $@
	for f in $(SCRIPTS); do sed -f ../lib/script2sgml.sed $$f >> $@; done
