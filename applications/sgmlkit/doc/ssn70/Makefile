# Makefile for ssn70 - documentation
#
# relies on env var STARLINK_SGML_DIR being set correctly
# $Id$
include $(STARLINK_SGML_DIR)/lib/Makefile.common

#JADEFLAGS=-V nochunks

ssn70.html.stamp: ssn70.sgml ssn70.back.stamp
ssn70.single.html: ssn70.sgml

index.html: ssn70.html.stamp
	ln -sf `awk -F: '{print $$1}' ssn70.html.stamp`.html index.html
