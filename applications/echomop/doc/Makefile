# Makefile for the defects list
#
# This uses software installed only on Norman's machine, and so is
#
#      NOT BUILDABLE AFTER DISTRIBUTION
#
# $Id$

LXDIR=/home/norman/sgml/lx
DEFECTSDIR=/home/norman/sgml/defect

defects.html: defects.xml
	java com.jclark.xsl.sax.Driver defects.xml $(DEFECTSDIR)/defects.xslt defects.html

