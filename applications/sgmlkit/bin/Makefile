# Expresses the dependencies of sgmlbin.sgml on its input files, and
# manages creating them from the script sources.
#
# $Id$

sgmlbin.sgml: sgml2docs.sgml sgml2xml.sgml sgml2hlp.sgml

%.sgml: %
	sed -f ../lib/script2sgml.sed $< > $@
