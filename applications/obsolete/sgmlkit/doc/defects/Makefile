# Makefile for the defects list
#
# This uses software installed only on Norman's machine, and so is
#
#      NOT BUILDABLE AFTER DISTRIBUTION
#
# $Id$

LXDIR=/home/norman/sgml/w/sgml/lx
DEFECTSDIR=/home/norman/sgml/w/sgml/defect

defects.html: defects.xml
	java com.jclark.xsl.sax.Driver defects.xml $(DEFECTSDIR)/defects.xslt defects.html

#defects.xslt: defects.lx
#	CLASSPATH=$(LXDIR) java lx.lx $(LXDIR)/xslt.lxdtd defects.lx > defects.xslt
