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
#     This file is included in the distribution for information only.
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
#     The files starlink-0.?.html and programcode-0.?.html are quite
#     distinct from this.  They're also generated from the
#     .description instances, and also contain all the description of
#     the elements in the DTD, but they also incorporate any
#     commentary included in that file, to make a single HTML file
#     which is intended to be consulted by people discussing the DTD.
#     These HTML files are generated using the XSLT transformation in
#     dtddoc.xslt.
#
#     How is that commentary included in the .description instance?
#     That's done using the dtddoc.pl script, which processes a
#     mailfile named sgml-starlink-0.7 in the directory sgml-mail.  It
#     breaks the messages in there into separate files in the
#     directory sgml-starlink-0.7, named after DTD elements, each of
#     which is a document fragment consisting of a sequence of
#     `commentary' elements as defined in the dtddescription DTD.
#     That's then spliced into the .description file using the Perl
#     script splice.pl.  That is, the sequence is:
#
#         sgml-mail/sgml-starlink-0.7           (mail archive)
#           -> sgml-starlink-0.7/elementname(s) (DTD fragments)
#           -> starlink-0.7.description         (using splice.pl)
#           -> starlink-0.7.html                (using dtddoc.xslt)
#
#     There's an identical sequence of actions for programcode-0.7
#
#     There are no special permissions required, since the MTA is set
#     up to deliver mail to the sgml-mail directory as user norman.
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

#include $(STARLINK_SGML_DIR)/lib/Makefile.common

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


# Not only is the following target not buildable after distribution,
# it is only buildable in a directory which has a file
# `sgml-mail/sgml-starlink-0.7' (since there is no rule for building this as a
# target).  That is, it's only usable in a directory which has been
# set up to receive the result of the dtddoc.pl script.  See notes at top.

# Create the HTML file from the DTD description
%.html: %.description
	CLASSPATH=$(CLASSPATH) $(JAVA) com.jclark.xsl.sax.Driver \
		$< dtddoc.xslt $@ listprefix=sgml-$(@:.html=)

# Splice the DTD fragments in the directory sgml-starlink-0.7 into the
# description file starlink-0.7.description
%.description: sgml-%
	for f in `./splice.pl $@ tmp-$@ $<`; do\
		rm $$f; \
	done
	mv $@ $@.old
	mv tmp-$@ $@

# Following is slightly tricky, since we want to leave the input file
# (sgml-mail/sgml-%) existing and of zero size, but still older than
# the target.
sgml-%: sgml-mail/sgml-%
	mv $< $(<)-temp
	touch $<
	./dtddoc.pl $@ < $(<)-temp
	cat $(<)-temp >> $<-full-archive
	rm $(<)-temp



starlink-0.7.html: starlink-0.7.description starlink-0.7.dtdsummary

starlink-0.7.description: sgml-starlink-0.7

sgml-starlink-0.7: sgml-mail/sgml-starlink-0.7

programcode-0.7.html: programcode-0.7.description programcode-0.7.dtdsummary

programcode-0.7.description: sgml-programcode-0.7

sgml-programcode-0.7: sgml-mail/sgml-programcode-0.7



#programcode-0.7.html: programcode-0.7.description programcode-0.7.dtdsummary programcode-0.7
#	if test programcode-0.7 -nt programcode-0.7.description; then \
#		echo 'splicing programcode-0.7';\
#		for f in `./splice.pl programcode-0.7.description tmp-programcode-0.7.description programcode-0.7`; do\
#			rm -f $$f; \
#		done; \
#		mv programcode-0.7.description programcode-0.7.description.old; \
#		mv tmp-programcode-0.7.description programcode-0.7.description; \
#	fi
#	CLASSPATH=$(CLASSPATH) $(JAVA) com.jclark.xsl.sax.Driver \
#		programcode-0.7.description dtddoc.xslt $@ listprefix=programcode-0.7





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
