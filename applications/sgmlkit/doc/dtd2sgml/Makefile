# $Id$

include $(STARLINK_SGML_DIR)/lib/Makefile.common

PERLSGML_DIR=/home/norman/s/src/sgml/misc/lib
DTDDIR=$(STARLINK_SGML_DIR)/dtd

%.dtdsummary: $(DTDDIR)/%.dtd
	SGML_SEARCH_PATH=$(DTDDIR) \
	perl -I $(PERLSGML_DIR) dtd2sgml.pl $< >$@ #2>/dev/null

%.text: %.dtdsummary
	jade -t sgml -d dtd2sgml.dsl $< >$@

# This stamp file is tested by ../ssn70/Makefile -- do not rename!
text.stamp: starlink-0.2.text programcode-0.2.text
	touch text.stamp

starlink-everything.dtdsummary: $(DTDDIR)/starlink-everything.dtd

starlink-0.2.text: starlink-0.2.description starlink-everything.dtdsummary

programcode-0.2.text: programcode-0.2.description programcode-0.2.dtdsummary

test.html.stamp: test.sgml programcode-0.2.text
