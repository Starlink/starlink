## Process this file with automake to produce Makefile.in

bin_SCRIPTS = bin/sgml2docs bin/sgml2hlp
nobase_dist_pkgdata_SCRIPTS = lib/img-eqlist.pl
# The contents of the dsssl/sl/latex/ directory are not installed, as they
# are no longer used (and may no longer work); the contents of dsssl/sl/xml
# are not installed as they are an experiment which was never completed.
nobase_dist_pkgdata_DATA = \
	lib/plainhtml.bst lib/plaintexml.bst \
	lib/postprocess-bbl.sed \
	lib/icons.tar \
	$(SL_COMMON) \
	$(SL_HLP) \
	$(SL_LIB) \
	$(SL_HTML) \
	$(SL_TEXML) \
	$(DTD_COLLECTION)

texmldir = $(pkglibdir)/lib/texml
dist_texml_SCRIPTS = \
	lib/texml/bin/texml.py
dist_texml_DATA = \
	lib/texml/bin/handler.py \
	lib/texml/bin/specmap.py \
	lib/texml/bin/texmlwr.py \
	lib/texml/bin/unimap.py

stardocs_DATA = @STAR_XML_DOCUMENTATION@

EXTRA_DIST = dsssl dtd

clean: clean-private
distclean: distclean-private
maintainer-clean: maintainer-clean-private
clean-private distclean-private maintainer-clean-private:
	@target=`echo $@ | sed s/-private//`; \
	  for d in dtd doc/dtd2sgml doc/ssn70 misc dsssl/sl/lib; do \
	    if test -d $$d; then \
	      (echo "In $$d..."; \
		cd $$d; test -f Makefile && $(MAKE) $$target); \
	    fi; \
	  done; \
	  $(MAKE) $$target-am

SL_COMMON = \
	dsssl/sl/common/sl-gentext.dsl \
	dsssl/sl/common/slcommon.dsl \
	dsssl/sl/common/slparams.dsl
SL_HLP = \
	dsssl/sl/hlp/slprogcode.dsl
SL_HTML = \
	dsssl/sl/html/sl.dsl \
	dsssl/sl/html/slback.dsl \
	dsssl/sl/html/sldocs.dsl \
	dsssl/sl/html/slhtml.dsl \
	dsssl/sl/html/sllinks.dsl \
	dsssl/sl/html/slmaths.dsl \
	dsssl/sl/html/slmisc.dsl \
	dsssl/sl/html/slnavig.dsl \
	dsssl/sl/html/slparams.dsl \
	dsssl/sl/html/slpreprocess.dsl \
	dsssl/sl/html/slroutines.dsl \
	dsssl/sl/html/slsect.dsl \
	dsssl/sl/html/slspecial.dsl \
	dsssl/sl/html/sltables.dsl
SL_LIB = \
	dsssl/sl/lib/dblib.dsl \
	dsssl/sl/lib/sllib.dsl
SL_TEXML = \
	dsssl/sl/texml/sl.dsl \
	dsssl/sl/texml/slback.dsl \
	dsssl/sl/texml/sldocs.dsl \
	dsssl/sl/texml/sllinks.dsl \
	dsssl/sl/texml/slmaths.dsl \
	dsssl/sl/texml/slmisc.dsl \
	dsssl/sl/texml/slparams.dsl \
	dsssl/sl/texml/slroutines.dsl \
	dsssl/sl/texml/slsect.dsl \
	dsssl/sl/texml/sltables.dsl
DTD_COLLECTION = \
    dtd/CATALOG \
    dtd/CATALOG-override \
    dtd/dsssl/catalog \
    dtd/dsssl/dsssl.dtd \
    dtd/dsssl/fot.dtd \
    dtd/dsssl/style-sheet.dtd \
    dtd/dssslcode-0.7.dtd \
    dtd/entities-html.ent \
    dtd/entities-latex.ent \
    dtd/entities-plain.ent \
    dtd/entities-unicode.ent \
    dtd/fortrancode-0.7.dtd \
    dtd/hytime/clause-C.2.txt \
    dtd/hytime/hi2mdga.sgm \
    dtd/hytime/hi2mdhyt.sgm-mod \
    dtd/hytime/hi2mdps.sgm \
    dtd/hytime/hytime-lextypes.sgm \
    dtd/hytime/sgml-lextypes.sgm \
    dtd/img-eqlist.dtd \
    dtd/ISOent/catalog \
    dtd/ISOent/ISOamsa.sdata \
    dtd/ISOent/ISOamsb.sdata \
    dtd/ISOent/ISOamsc.sdata \
    dtd/ISOent/ISOamsn.sdata \
    dtd/ISOent/ISOamso.sdata \
    dtd/ISOent/ISOamsr.sdata \
    dtd/ISOent/ISObox.sdata \
    dtd/ISOent/ISOcyr1.sdata \
    dtd/ISOent/ISOcyr2.sdata \
    dtd/ISOent/ISOdia.sdata \
    dtd/ISOent/ISOgrk1.sdata \
    dtd/ISOent/ISOgrk2.sdata \
    dtd/ISOent/ISOgrk3.sdata \
    dtd/ISOent/ISOgrk4.sdata \
    dtd/ISOent/ISOlat1-ent.ent \
    dtd/ISOent/ISOlat1-html.ent \
    dtd/ISOent/ISOlat1-latex.ent \
    dtd/ISOent/ISOlat1.sdata \
    dtd/ISOent/ISOlat2.sdata \
    dtd/ISOent/ISOnum.sdata \
    dtd/ISOent/ISOpub.sdata \
    dtd/ISOent/ISOtech.sdata \
    dtd/ISOent/README \
    dtd/ISOent/xml/iso-amsa.ent \
    dtd/ISOent/xml/iso-amsb.ent \
    dtd/ISOent/xml/iso-amsc.ent \
    dtd/ISOent/xml/iso-amsn.ent \
    dtd/ISOent/xml/iso-amso.ent \
    dtd/ISOent/xml/iso-amsr.ent \
    dtd/ISOent/xml/iso-box.ent \
    dtd/ISOent/xml/iso-cyr1.ent \
    dtd/ISOent/xml/iso-cyr2.ent \
    dtd/ISOent/xml/iso-dia.ent \
    dtd/ISOent/xml/iso-grk1.ent \
    dtd/ISOent/xml/iso-grk2.ent \
    dtd/ISOent/xml/iso-grk3.ent \
    dtd/ISOent/xml/iso-grk4.ent \
    dtd/ISOent/xml/iso-lat1.ent \
    dtd/ISOent/xml/iso-lat2.ent \
    dtd/ISOent/xml/iso-num.ent \
    dtd/ISOent/xml/iso-pub.ent \
    dtd/ISOent/xml/iso-tech.ent \
    dtd/mud-0.7.dtd \
    dtd/mud-xml-0.7.dtd \
    dtd/notations.text \
    dtd/oasis-exchange-table-xml.dtd \
    dtd/oasis-exchange-table.dtd \
    dtd/paracontent-0.7.dtd \
    dtd/programcode-0.7.dtd \
    dtd/sc-0.7.dtd \
    dtd/sc-xml-0.7.dtd \
    dtd/scriptcode-0.7.dtd \
    dtd/sg-0.7.dtd \
    dtd/sg-xml-0.7.dtd \
    dtd/sgp-0.7.dtd \
    dtd/sgp-xml-0.7.dtd \
    dtd/simple-0.7.dtd \
    dtd/ssn-0.7.dtd \
    dtd/ssn-xml-0.7.dtd \
    dtd/starhtml.dtd \
    dtd/starlink-0.7.dtd \
    dtd/starlink-everything.dtd \
    dtd/starlink.decl \
    dtd/sug-0.7.dtd \
    dtd/sug-xml-0.7.dtd \
    dtd/summary-0.7.dtd \
    dtd/sun-0.7.dtd \
    dtd/sun-xml-0.7.dtd \
    dtd/tth-equations.dtd \
    dtd/xhtml1/xhtml-lat1.ent \
    dtd/xhtml1/xhtml-special.ent \
    dtd/xhtml1/xhtml-symbol.ent \
    dtd/xhtml1/xhtml.soc \
    dtd/xhtml1/xhtml1-frameset.dtd \
    dtd/xhtml1/xhtml1-strict.dtd \
    dtd/xhtml1/xhtml1-transitional.dtd \
    dtd/xhtml1/xhtml1.dcl \
    dtd/xml.decl

# doc/ssn70/ssn70.{htx,texml}_tar are distributed, but their
# dependencies are not, so arrange that the dependencies are
# made/checked only if the target isn't present.  This breaks the
# make dependency between the tarballs and the version number in
# configure.ac, and so can lose if the distribution is built from a
# checkout rather than an export; but that's not supposed to happen.
doc/ssn70/ssn70.htx_tar:
	$(MAKE) doc/ssn70/sgmlkit-version-string doc/dtd2sgml/Makefile
	cd doc/ssn70 && $(MAKE) ssn70.htx_tar
doc/ssn70/ssn70.texml_tar:
	$(MAKE) doc/ssn70/sgmlkit-version-string doc/dtd2sgml/Makefile
	cd doc/ssn70 && $(MAKE) ssn70.texml_tar
doc/ssn70/sgmlkit-version-string: configure.ac
	echo @PACKAGE_VERSION@ >doc/ssn70/sgmlkit-version-string
# Substitute doc/dtd2sgml/Makefile.in by hand -- it's not distributed,
# so can't be mentioned in AC_CONFIG_FILES
doc/dtd2sgml/Makefile: doc/dtd2sgml/Makefile.in
	rm -f $@ $@.tmp
	sed 's,@JADE\@,$(JADE),;s,@PERL\@,$(PERL),;s,@PERLSGMLDIR\@,$(PERLSGMLDIR),' $(srcdir)/$< >$@.tmp
	mv $@.tmp $@

# The sgml2docs and sgml2hlp scripts contain references to the
# installation directories pkglibdir, and so must be substituted here
# at make time, rather than by ./configure at configuration time.  See
# section `Installation Directory Variables' in the autoconf manual
# for discussion.  The source files *.x here have been substituted
# from *.in by ./configure.
edit=sed -e 's,!!pkglibdir!!,$(pkglibdir),g'
bin/sgml2docs: bin/sgml2docs.x
	rm -f $@ $@.tmp
	$(edit) $(srcdir)/$@.x >$@.tmp
	chmod +x $@.tmp
	mv $@.tmp $@
bin/sgml2hlp: bin/sgml2hlp.x
	rm -f $@ $@.tmp
	$(edit) $(srcdir)/$@.x >$@.tmp
	chmod +x $@.tmp
	mv $@.tmp $@

dtd/CATALOG-override:
	cd dtd; make CATALOG-override

dsssl/sl/lib/sllib.dsl:
	cd dsssl/sl/lib; make sllib.dsl
