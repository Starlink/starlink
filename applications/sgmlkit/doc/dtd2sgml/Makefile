# $Id$
#
# Note that this has a hard-coded directory path, which points to a
# perlSGML installation.  This isn't included in the distribution system, so
# the targets in this directory ARE NOT BUILDABLE after distribution.

include $(STARLINK_SGML_DIR)/lib/Makefile.common

PERLSGML_DIR=/home/norman/s/src/sgml/misc/lib
DTDDIR=$(STARLINK_SGML_DIR)/dtd

%.dtdsummary: $(DTDDIR)/%.dtd
	SGML_SEARCH_PATH=$(DTDDIR) \
	perl -I $(PERLSGML_DIR) dtd2sgml.pl $< >$@ #2>/dev/null

%.sgmltext: %.dtdsummary
	jade -t sgml -d dtd2sgml.dsl $< >$@

all:
	echo "Error: this directory is included in the distribution for"
	echo "reference only.  It uses a package (perlSGML) which is not"
	echo "used in the built package, and so it is NOT BUILDABLE"
	echo "after distribution"

# This existence of this stamp file is tested by ../ssn70/Makefile -- 
# do not rename!
sgmltext.stamp: starlink-0.4.sgmltext programcode-0.2.sgmltext
	touch sgmltext.stamp

starlink-0.2.dtdsummary: $(DTDDIR)/starlink-everything.dtd $(DTDDIR)/starlink-0.2.dtd
	SGML_SEARCH_PATH=$(DTDDIR) \
	perl -I $(PERLSGML_DIR) dtd2sgml.pl --descrip starlink-0.2.description \
		$< >$@ 2>/dev/null

starlink-0.4.dtdsummary: $(DTDDIR)/starlink-everything.dtd \
			$(DTDDIR)/starlink-0.4.dtd
	SGML_SEARCH_PATH=$(DTDDIR) \
	perl -I $(PERLSGML_DIR) dtd2sgml.pl \
			--descrip starlink-0.4.description \
		$< >$@ 2>/dev/null

starlink-0.3.dtdsummary: $(DTDDIR)/starlink-everything.dtd \
		$(DTDDIR)/starlink-0.3.dtd
	SGML_SEARCH_PATH=$(DTDDIR) \
	perl -I $(PERLSGML_DIR) dtd2sgml.pl --descrip starlink-0.3.description \
		$< >$@ 2>/dev/null

starlink-0.2.sgmltext: starlink-0.2.dtdsummary starlink-0.2.description

starlink-0.3.sgmltext: starlink-0.3.dtdsummary starlink-0.3.description

starlink-0.4.sgmltext: starlink-0.4.dtdsummary starlink-0.4.description

programcode-0.2.sgmltext: programcode-0.2.dtdsummary programcode-0.2.description

test.html.stamp: test.sgml programcode-0.2.sgmltext

clean:
	rm -f *.sgmltext *.dtdsummary sgmltext.stamp
