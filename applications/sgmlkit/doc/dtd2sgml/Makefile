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
#     (b) it uses the perlSGML distribution, and (c) it uses a
#     Java distribution plus xt, none of which is distributed with the
#     SGML Kit.  Therefore:
#
#        THE CONTENTS OF THIS DIRECTORY ARE NOT BUILDABLE AFTER DISTRIBUTION
#
#
#     Explanations:
#
#     There's quite a lot happening in this directory, most of which is
#     magic.  The main target in this makefile is sgmltext.stamp, but
#     there's another target in general-0.?.html and programcode-0.?.html
#     which is not directly incorporated into the documentation.
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
#     The files general-0.?.html and programcode-0.?.html are quite
#     distinct from this.  They're also generated from the
#     .description instances, and also contain all the description of
#     the elements in the DTD, but they also incorporate any
#     commentary included in that file, to make a single HTML file
#     which is intended to be consulted by people discussing the DTD.
#     These HTML files are generated using the XSLT transformation in
#     dtddoc.xslt.
#
#     How is that commentary included in the .description instance?
#     That's done using the dtddoc.pl script, which is the
#     right-hand-side of a mail alias such as
#
#         general-0.6: |/path-to-here/dtddoc.pl general-0.6
#
#     That takes an email message, and writes it out as a document
#     fragment, consisting of a sequence of `commentary' elements as
#     defined in the dtddescription DTD, within a subdirectory called
#     general-0.6.  That's then spliced into the .description file
#     using the Perl script splice.pl
#
#     The `general-0.6' subdirectory has to be writable by the mailer
#     (ie, group `exim', and group-writable), but it also has to be
#     writable by the owner of this script, so that it can delete
#     files in that subdirectory once they've been spliced into the
#     .description file.
#
#  Authors:
#     NG: Norman Gray (Starlink, Glasgow)
#
#  History:
#     Autumn 1999 (NG):
#        Initial version
#     31-Mar-2000 (NG):
#        Revised to include general-0.?.html and programcode-0.?.html targets
#
#  RCS Id:
#     $Id$
#-

include $(STARLINK_SGML_DIR)/lib/Makefile.common

PERLSGML_DIR=/home/norman/s/src/sgml/misc/lib
DTDDIR=$(STARLINK_SGML_DIR)/dtd
DECL=$(STARLINK_SGML_DIR)/dtd/xml.decl
JAVA=/data/goedel/norman/packages/java/blackdown/jdk118_v1/bin/java
CLASSPATH=/data/goedel/norman/s/src/sgml/java/classes/xp.jar:/data/goedel/norman/s/src/sgml/java/classes/xt.jar:/data/goedel/norman/s/src/sgml/java/classes/sax.jar

%.dtdsummary: $(DTDDIR)/%.dtd
	SGML_CATALOG_FILES=./CATALOG:$(DTDDIR)/CATALOG \
	SGML_SEARCH_PATH=$(DTDDIR) \
	perl -I $(PERLSGML_DIR) dtd2sgml.pl $< >$@ 

%.sgmltext: %.description %.dtdsummary
	jade -t sgml -d dtd2sgml.dsl $(DECL) $< >$@

# Handy target for producing a skeleton description of a DTD, to
# start on the documentation task.
%.description.skel:
	SGML_SEARCH_PATH=$(DTDDIR) \
	perl -I $(PERLSGML_DIR) dtdskeleton.pl \
		$(DTDDIR)/$(@:.description.skel=.dtd) >$@



# Default target must fail with an informative error message

all:
	echo "Error: this directory is included in the distribution for"
	echo "reference only.  It uses a package (perlSGML) which is not"
	echo "used in the built package, and so it is NOT BUILDABLE"
	echo "after distribution"

# This is the main target of this makefile
#
# The existence of this stamp file is tested by ../ssn70/Makefile -- 
# do not rename!
sgmltext.stamp: starlink-0.6.sgmltext programcode-0.6.sgmltext
	touch sgmltext.stamp

starlink-0.6.dtdsummary: $(DTDDIR)/starlink-everything.dtd \
			$(DTDDIR)/starlink-0.6.dtd
	SGML_CATALOG_FILES=./CATALOG:$(DTDDIR)/CATALOG \
	SGML_SEARCH_PATH=$(DTDDIR) \
	perl -I $(PERLSGML_DIR) dtd2sgml.pl \
		$< >$@


general-0.6.html: starlink-0.6.description
	CLASSPATH=$(CLASSPATH) $(JAVA) com.jclark.xsl.sax.Driver \
		$< dtddoc.xslt $@ listprefix=general-0.6

starlink-0.6.description: general-0.6
	for f in `./splice.pl $@ tmp-$@ general-0.6`; do\
		rm $$f; \
	done
	mv $@ $@.old
	mv tmp-$@ $@

clean:
	rm -f *.sgmltext *.dtdsummary sgmltext.stamp


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
