#+
#
#  Name:
#    Makefile
#
#  Type of module:
#    makefile
#
#  Description:
#
#     This directory contains the documentation for the DTD, massaged into
#     a form which can be scooped into the main document SSN/70 in
#     directory ../ssn70.  It has to be built when the distribution is
#     being constructed, since (a) it includes hard-coded directory paths,
#     and (b) it uses the perlSGML distribution.
#
#        THE CONTENTS OF THIS DIRECTORY ARE NOT BUILDABLE AFTER DISTRIBUTION
#
#     This file is included in the distribution for information only.
#
#
#
#     Explanations:
#
#     There's quite a lot happening in this directory, most of which is
#     magic.  The main target in this makefile is sgmltext.stamp.
#
#     sgmltext.stamp depends on the two targets starlink-0.?.sgmltext and
#     programcode-0.?.sgmltext, which are fragments conforming to the SSN
#     DTD, and are intended to be included in ../ssn70/ssn70.sgml
#
#     The .dtdsummary files are generated directly from the DTD using
#     perlSGML.  These are then included as entities within the file
#     .description, which, with that inclusion, is a valid instance of
#     the DTD dtddescription.dtd.  That instance is processed with the
#     dtd2sgml.dsl DSSSL stylesheet to produce the .sgmltext file.
#
#
#  Authors:
#     NG: Norman Gray (Starlink, Glasgow)
#
#  History:
#     Autumn 1999 (NG):
#        Initial version
#     31-Mar-2000 (NG):
#        Revised to include general-0.?.html and programcode-0.?.html targets
#     5-Jun-2000 (NG):
#        Revised to _remove_ the *.html targets
#
#  RCS Id:
#     $Id$
#-


PERLSGML_DIR=/home/norman/s/src/sgml/misc/lib
DTDDIR=$(STARLINK_SGML_DIR)/dtd
DECL=$(STARLINK_SGML_DIR)/dtd/xml.decl

%.dtdsummary: $(DTDDIR)/%.dtd
	SGML_CATALOG_FILES=./CATALOG:$(DTDDIR)/CATALOG \
	SGML_SEARCH_PATH=$(DTDDIR) \
	perl -I $(PERLSGML_DIR) dtd2sgml.pl $< >$@ 

%.sgmltext: %.description
	jade -t sgml -d dtd2sgml.dsl $(DECL) $< >$@

# Handy target for producing a skeleton description of a DTD, to
# start on the documentation task.
%.description.skel:
	SGML_SEARCH_PATH=$(DTDDIR) \
	perl -I $(PERLSGML_DIR) dtdskeleton.pl \
		$(DTDDIR)/$(@:.description.skel=.dtd) >$@



# Default target must fail with an informative error message

all:
	@ echo "Error: this directory is included in the distribution for"
	@ echo "reference only.  It uses a package (perlSGML) which is not"
	@ echo "used in the built package, and so it is NOT BUILDABLE"
	@ echo "after distribution.  See notes within Makefile."

# This is the main target of this makefile
#
# The existence of this stamp file is tested by ../ssn70/Makefile -- 
# do not rename!
sgmltext.stamp: starlink-0.7.sgmltext programcode-0.7.sgmltext
	touch sgmltext.stamp

# Because the two .description files cross-refer to each other, source files
# for both must be up-to-date before either can be processed.
starlink-0.7.sgmltext: starlink-0.7.description \
		starlink-0.7.dtdsummary \
		programcode-0.7.description \
		programcode-0.7.dtdsummary

programcode-0.7.sgmltext: programcode-0.7.description \
		programcode-0.7.dtdsummary \
		starlink-0.7.description \
		starlink-0.7.dtdsummary

starlink-0.7.dtdsummary: $(DTDDIR)/starlink-everything.dtd \
			$(DTDDIR)/starlink-0.7.dtd
	SGML_CATALOG_FILES=./CATALOG:$(DTDDIR)/CATALOG \
	SGML_SEARCH_PATH=$(DTDDIR) \
	perl -I $(PERLSGML_DIR) dtd2sgml.pl \
		$< >$@

# No longer used (here for reference)
starlink-0.6.dtdsummary: $(DTDDIR)/starlink-everything.dtd \
			$(DTDDIR)/starlink-0.6.dtd
	SGML_CATALOG_FILES=./CATALOG:$(DTDDIR)/CATALOG \
	SGML_SEARCH_PATH=$(DTDDIR) \
	perl -I $(PERLSGML_DIR) dtd2sgml.pl \
		$< >$@


clean:
	rm -f *.sgmltext *.dtdsummary sgmltext.stamp
