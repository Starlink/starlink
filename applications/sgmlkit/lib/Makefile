# Expresses the dependencies of sgmllib.sgml on its input files, and
# manages creating them from the script sources.
#
# $Id$

sgmllib.sgml: img-eqlist.pl.sgml

%.sgml: %
	sed -f script2sgml.sed $< > $@

