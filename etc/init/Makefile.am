## Process this file with automake to produce Makefile.in

## Scripts which end up in the binary installation directory.

staretc_DATA = not_installed icl_login_sys.icl login cshrc profile
staretc_SCRIPTS = notinst

starhelp_DATA = adam_packages.shl

EXTRA_DIST = cshrc.in notinst not_installed icl_login_sys.icl login.in adam_packages.hlp \
             profile.in

## Root for the STARJAVA installation. Define like this so that it can
## be changed at make time, so some position that doesn't depend on
## exec_prefix.
STARJAVADIR = $(exec_prefix)/starjava

## Arrange to edit the installation binary directory into script files
## which need it.  This cannot be done by autoconf because it needs to
## be changable at make time - see the autoconf manual section on
## Installation Directory Variables.
## Since these ones haven't been modified by autoconf, we take the
## opportunity to do any non-make-sensitive substitutions here as well.

LD_LIB_PATH = @LD_LIB_PATH@

edit = sed -e 's%@bindir\@%$(bindir)%g' \
           -e 's%@staretcdir\@%$(staretcdir)%g' \
           -e 's%@starhelpdir\@%$(starhelpdir)%g' \
           -e 's%@libdir\@%$(libdir)%g' \
           -e 's%@datadir\@%$(datadir)%g' \
           -e 's%@sharelibdir\@%$(libdir)%g' \
           -e 's%@datadir\@%$(datadir)%g' \
           -e 's%@VERSION\@%$(VERSION)%g' \
           -e 's%@mandir\@%$(mandir)%g' \
           -e 's%@starjavadir\@%$(STARJAVADIR)%g' \
           -e 's%@exec_prefix\@%$(exec_prefix)%g' \
           -e 's%@build\@%$(build)%g' \
           -e 's%@LD_LIB_PATH\@%$(LD_LIB_PATH)%g'

login:  Makefile $(srcdir)/login.in
	rm -f login login.tmp
	$(edit) $(srcdir)/login.in >login.tmp
	mv login.tmp login

cshrc:  Makefile $(srcdir)/cshrc.in
	rm -f cshrc cshrc.tmp
	$(edit) $(srcdir)/cshrc.in >cshrc.tmp
	mv cshrc.tmp cshrc

profile:  Makefile $(srcdir)/profile.in
	rm -f profile profile.tmp
	$(edit) $(srcdir)/profile.in >profile.tmp
	mv profile.tmp profile

.hlp.shl:
	@HLIB@ $<

CLEANFILES = adam_packages.shl cshrc login profile

dist_starnews_DATA = init.news
