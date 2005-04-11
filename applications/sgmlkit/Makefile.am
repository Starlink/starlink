## Process this file with automake to produce Makefile.in

bin_SCRIPTS = bin/sgml2docs bin/sgml2hlp
nobase_dist_pkglib_SCRIPTS = lib/img-eqlist.pl
nobase_dist_pkglib_DATA = \
	lib/plainhtml.bst lib/plainlatex.bst lib/postprocess-bbl.sed \
	lib/icons.tar \
	$(SL_COMMON) \
	$(SL_HLP) \
	$(SL_LIB) \
	$(SL_HTML) \
	$(SL_TEXML)

texmldir = $(pkglibdir)/lib/texml
dist_texml_SCRIPTS = \
	lib/texml/bin/texml.py
dist_texml_DATA = \
	lib/texml/bin/handler.py \
	lib/texml/bin/specmap.py \
	lib/texml/bin/texmlwr.py \
	lib/texml/bin/unimap.py

stardocs_DATA = @STAR_XML_DOCUMENTATION@

# The list of DTDs to install is maintained within the dtd/ directory,
# in file dtd/manifest.  Install them by copying the tree into place.
install-data-local: dtd/manifest
	$(mkdir_p) $(DESTDIR)$(pkglibdir)
	if test -n "$(PAX)"; then \
	  if $(MANIFEST); then \
	    $(PAX) -r -w -v $(DESTDIR)$(pkglibdir) \
		<dtd/manifest 2>&1 | sed 's/^/MANIFEST:/'; \
	  else \
	    $(PAX) -r -w $(DESTDIR)$(pkglibdir) \
		<dtd/manifest; \
	  fi; \
	else \
	    cat dtd/manifest | while read f; do \
	        $(INSTALL_DATA) $$f $(DESTDIR)$(pkglibdir)/$$f; \
	    done; \
	    $(MANIFEST) && sed 's+^+MANIFEST:$(DESTDIR)$(pkglibdir)/+' dtd/manifest || :; \
	fi

## inclusion of dtd/manifest here ensures that this is built before dist
EXTRA_DIST = dsssl dtd dtd/manifest

clean: clean-private
distclean: distclean-private
maintainer-clean: maintainer-clean-private
clean-private distclean-private maintainer-clean-private:
	@target=`echo $@ | sed s/-private//`; \
	  for d in dtd doc/dtd2sgml doc/ssn70 misc dsssl/sl/lib; do \
	    if test -d $$d; then \
	      (echo "In $$d..."; 
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

dtd/manifest: dtd/CATALOG dtd/manifest-omit
	cd dtd; make manifest

dsssl/sl/lib/sllib.dsl:
	cd dsssl/sl/lib; make sllib.dsl
