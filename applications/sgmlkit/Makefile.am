## Process this file with automake to produce Makefile.in

bin_SCRIPTS = bin/sgml2docs bin/sgml2hlp
nobase_pkglib_SCRIPTS = lib/img-eqlist.pl
nobase_pkglib_DATA = \
	lib/plainhtml.bst lib/plainlatex.bst lib/postprocess-bbl.sed \
	lib/icons.tar \
	$(SL_COMMON) \
	$(SL_HLP) \
	$(SL_LIB) \
	$(SL_HTML) \
	$(SL_TEXML)

stardocs_DATA = doc/ssn70/ssn70.htx_tar

# The list of DTDs to install is maintained within the dtd/ directory,
# in file dtd/manifest.  Install them by copying the tree into place.
install-data-local: dtd/manifest
	$(mkdir_p) $(DESTDIR)$(pkglibdir)
	@PAX@ -r -w -v $(DESTDIR)$(pkglibdir) \
		<dtd/manifest \
		2>>$${MANIFEST_FILE=/dev/null}

EXTRA_DIST = dsssl dtd

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
	dsssl/sl/lib/sllib-jade-1.2.1.dsl \
	dsssl/sl/lib/sllib-openjade-1.3.dsl
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

doc/ssn70/ssn70.htx_tar:
	cd doc/ssn70 && $(MAKE) ssn70.htx_tar

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

dtd/manifest:
	cd dtd; make manifest
