# $Id$
#
# Note that this has a hard-coded directory path, which points to a
# perlSGML installation.  This isn't included in the distribution system, so
# the targets in this directory ARE NOT BUILDABLE after distribution.

include $(STARLINK_SGML_DIR)/lib/Makefile.common

PERLSGML_DIR=/home/norman/s/src/sgml/misc/lib
DTDDIR=$(STARLINK_SGML_DIR)/dtd
DECL=$(STARLINK_SGML_DIR)/dtd/starlink.decl

%.dtdsummary: $(DTDDIR)/%.dtd
	SGML_SEARCH_PATH=$(DTDDIR) \
	perl -I $(PERLSGML_DIR) dtd2sgml.pl $< >$@ 
#2>/dev/null

%.sgmltext: %.description
	jade -t sgml -d dtd2sgml.dsl $(DECL) $< >$@

# Handy target for producing a skeleton description of a DTD, to
# start on the documentation task.
%.description.skel:
	SGML_SEARCH_PATH=$(DTDDIR) \
	perl -I $(PERLSGML_DIR) dtdskeleton.pl \
		$(DTDDIR)/$(@:.description.skel=.dtd) >$@

all:
	echo "Error: this directory is included in the distribution for"
	echo "reference only.  It uses a package (perlSGML) which is not"
	echo "used in the built package, and so it is NOT BUILDABLE"
	echo "after distribution"

# This existence of this stamp file is tested by ../ssn70/Makefile -- 
# do not rename!
sgmltext.stamp: starlink-0.6.sgmltext programcode-0.6.sgmltext
	touch sgmltext.stamp

starlink-0.6.dtdsummary: $(DTDDIR)/starlink-everything.dtd \
			$(DTDDIR)/starlink-0.6.dtd
	SGML_SEARCH_PATH=$(DTDDIR) \
	perl -I $(PERLSGML_DIR) dtd2sgml.pl \
		$< >$@
#2>/dev/null

programcode-0.6.sgmltext: programcode-0.6.description programcode-0.6.dtdsummary

# The way that I produce the documetation changed during the
# preparations for release 0.6, and these following targets will
# probably no longer work.
#
# starlink-0.5.dtdsummary: $(DTDDIR)/starlink-everything.dtd \
# 			$(DTDDIR)/starlink-0.5.dtd
# 	SGML_SEARCH_PATH=$(DTDDIR) \
# 	perl -I $(PERLSGML_DIR) dtd2sgml.pl \
# 			--descrip starlink-0.5.description \
# 		$< >$@ 2>/dev/null
#
# starlink-0.4.dtdsummary: $(DTDDIR)/starlink-everything.dtd \
# 			$(DTDDIR)/starlink-0.4.dtd
# 	SGML_SEARCH_PATH=$(DTDDIR) \
# 	perl -I $(PERLSGML_DIR) dtd2sgml.pl \
# 			--descrip starlink-0.4.description \
# 		$< >$@ 2>/dev/null
#
# starlink-0.3.dtdsummary: $(DTDDIR)/starlink-everything.dtd \
# 		$(DTDDIR)/starlink-0.3.dtd
# 	SGML_SEARCH_PATH=$(DTDDIR) \
# 	perl -I $(PERLSGML_DIR) dtd2sgml.pl --descrip starlink-0.3.description \
# 		$< >$@ 2>/dev/null
#
# starlink-0.2.dtdsummary: $(DTDDIR)/starlink-everything.dtd $(DTDDIR)/starlink-0.2.dtd
# 	SGML_SEARCH_PATH=$(DTDDIR) \
# 	perl -I $(PERLSGML_DIR) dtd2sgml.pl --descrip starlink-0.2.description \
# 		$< >$@ 2>/dev/null
#
# starlink-0.2.sgmltext: starlink-0.2.dtdsummary starlink-0.2.description
#
# starlink-0.3.sgmltext: starlink-0.3.dtdsummary starlink-0.3.description
#
# starlink-0.4.sgmltext: starlink-0.4.dtdsummary starlink-0.4.description
#
#starlink-0.5.sgmltext: starlink-0.5.dtdsummary starlink-0.5.description
#
# programcode-0.2.sgmltext: programcode-0.2.description programcode-0.2.dtdsummary

#test.html.stamp: test.sgml programcode-0.2.sgmltext

#clean:
#	rm -f *.sgmltext *.dtdsummary sgmltext.stamp
