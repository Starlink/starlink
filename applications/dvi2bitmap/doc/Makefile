# dvi2bitmap documentation makefile
#
# Relies on STARLINK_SGML_DIR being set correctly.
#
# $Id$

DOCTARBALLS=doc.tar-htx doc.tar-latex doc.tar-htxsingle

doc.tar-htx: dvi2bitmap.sgml
	$(STARLINK_SGML_DIR)/bin/sgml2docs --html \
		--packagedir ssn71.htx --tarball $@ $<

doc.tar-latex: dvi2bitmap.sgml
	$(STARLINK_SGML_DIR)/bin/sgml2docs --latex \
		--packagedir ssn71.latex --tarball $@ $<

doc.tar-htxsingle: dvi2bitmap.sgml
	$(STARLINK_SGML_DIR)/bin/sgml2docs --htmlsingle \
		--packagedir ssn71.htxsingle --tarball $@ $<

doc.tar: $(DOCTARBALLS)
	test -d doc || mkdir doc
	cd doc								;\
		for i in $(DOCTARBALLS); do 				\
			tar xf ../$$i					;\
		done							;\
		mv ssn71.latex/ssn71.tex ./dvi2bitmap.tex 		;\
		mv ssn71.htx/* .					;\
		mv ssn71.htxsingle/index.html ./dvi2bitmap.single.html	;\
		rm -Rf ssn71.* sgmlnorm.errors MANIFEST			;\
		ln ../dvi2bitmap.sgml dvi2bitmap.sgml
	tar cf $@ doc
	rm -Rf doc

clean:
	rm -f doc.tar $(DOCTARBALLS)
	rm -Rf ssn71.htx ssn71.latex ssn71.htxsingle ssn71.tex
	rm -Rf dvi2bitmap_doc
