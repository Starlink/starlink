# Makefile for ssn70 - documentation
#
# relies on env var STARLINK_SGML_DIR being set correctly
# $Id$
#
include $(STARLINK_SGML_DIR)/lib/Makefile.common

#JADEFLAGS=-V nochunks
#JADEFLAGS=-V show-element-ids

# prepare a complete set of documentation, ensuring that all the documents 
# linked to in the HTML are present.  Put the complete list in docset.manifest
docset.manifest: ssn70.html.stamp ssn70.latex.stamp
	cat HTML.manifest LaTeX.manifest >tmp.manifest
	for f in `awk -F: '{split($$2,a," "); for (i in a) print $$1"."a[i];}' ssn70.html.stamp`; \
	do \
		$(MAKE) $$f; \
		echo $$f >>tmp.manifest; \
	done
	# remove duplicates
	sort tmp.manifest | uniq > docset.manifest
	rm -f tmp.manifest

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
