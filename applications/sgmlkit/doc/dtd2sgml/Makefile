# $Id$

PERLSGML_DIR=/home/norman/s/src/sgml/misc/lib
DTDDIR=$(STARLINK_SGML_DIR)/dtd

%.dtdsummary: $(DTDDIR)/%.dtd
	SGML_SEARCH_PATH=$(DTDDIR) \
	perl -I $(PERLSGML_DIR) dtd2sgml.pl $< >$@ #2>/dev/null

starlink-everything.dtdsummary: $(DTDDIR)/starlink-everything.dtd
