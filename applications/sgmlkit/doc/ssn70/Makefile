# Makefile for ssn70 - documentation
#
# relies on env var STARLINK_SGML_DIR being set correctly
# $Id$
#
include $(STARLINK_SGML_DIR)/lib/Makefile.common

#JADEFLAGS=-V nochunks
JADEFLAGS=-V show-element-ids

ssn70.html.stamp: ssn70.sgml ssn70.htmlback.stamp \
	../dtd2sgml/sgmltext.stamp ../../ReleaseNotes
ssn70.single.html: ssn70.sgml
ssn70.latex.stamp: ssn70.latexback.stamp \
	../dtd2sgml/sgmltext.stamp ../../ReleaseNotes
ssn70.latexback.stamp: ../dtd2sgml/sgmltext.stamp
ssn70.htmlback.stamp: ../dtd2sgml/sgmltext.stamp

../dtd2sgml/sgmltext.stamp:
	cd ../dtd2sgml; $(MAKE) sgmltext.stamp

index.html: ssn70.html.stamp
	ln -sf `awk -F: '{print $$1}' ssn70.html.stamp`.html index.html
