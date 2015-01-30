#  List of public script files.  These are scripts which form part of
#  the package and will be required by users of it.  They will be
#  installed in the $(INSTALL_BIN) directory with execute permission
#  set.

PUBLIC_SCR = hlink findme showme

#  List of private scripts, not usually seen by the user. These will be
#  installed in the $(INSTALL_BIN_PKG) directory.

PRIVATE_SCR =  allfind browse creindex depends docfind dockey findbrowser \
gettitle homepage htxpath moveddocs msgover multised newdocs newindex \
oldindex reffind relink resolve settrap showhtml stdfile urlgen wrfiles \
testdocs obsdoc_adsurl.txt

EXTRA_DIST = creindex.sed.in edhead.sed.in edtail.sed.in gethead.sed.in \
             gettitle.sed.in htxpath.in

#  List of private sed scripts (N.B. these contain comments which MUST be
#  removed before use).

PRIVATE_SED_SCR = creindex.sed edhead.sed edtail.sed gethead.sed \
gettitle.sed

SUFFIXES = .sed.in .sed

.sed.in.sed:
	@SED@ -e '/^#/d' \
              -e '/^[  ]*$$/d' $< > $*.sed

dist_bin_SCRIPTS = $(PUBLIC_SCR)

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

htxsupportdir = $(bindir)/htx-scripts
dist_htxsupport_SCRIPTS = $(PRIVATE_SCR)
dist_htxsupport_DATA = $(PRIVATE_SED_SCR)

htxpath: $(srcdir)/htxpath.in Makefile
	rm -f htxpath htxpath.tmp
	@SED@ -e 's%|INSTALL|%$(prefix)%' \
              -e 's%|STARLINK|%$(STARLINK)%' \
              $(srcdir)/htxpath.in >htxpath.tmp
	chmod +x htxpath.tmp
	mv htxpath.tmp htxpath
              

BUILT_SOURCES = $(PRIVATE_SED_SCR) htxpath

# Files to be removed when make clean is run

CLEANFILES = $(PRIVATE_SED_SCR) htxpath

dist_starnews_DATA = htx.news


