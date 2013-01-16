## Process this file with automake to produce Makefile.in

bin_SCRIPTS = dbmcat.pl scbcp.pl scbindex.pl scb.pl scb.csh

bin_PROGRAMS = ctag ftag
ctag_SOURCES = ctag-y.y tag.c tag.h
ftag_SOURCES = ftag-y.y tag.c tag.h

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

EXTRA_DIST = Scb.pm.in YyTag.pm.in scb.csh.in scb.pl.in scbindex.pl.in \
             scbcp.pl.in dbmcat.pl.in ctag-l.c ftag-l.l test_frame.c \
             StarIndex.pm

supportdir = $(bindir)
dist_support_DATA = mime.types

AM_YFLAGS = -d

BUILT_SOURCES = Scb.pm YyTag.pm scb.csh scb.pl scbindex.pl scbcp.pl dbmcat.pl \
                ctag-y.h ftag-y.h ctag-l.c ftag-l.c

# Run these scripts as a test
TESTS = scb_test.sh size_test.sh

# Define and distribute the test data
dist_check_DATA = scb_test.tar

scb_test.tar:
	tar cf $@ \
                  test/Makefile \
                  test/scb_c_test.c \
                  test/scb_f_test.f \
                  test/scb_locase.c

## Since most of the substitions that have to be done are related to
## the installation directory, we do them all here in the cumbersome way,
## since you can't get autoconf alone to handle substitutions related
## to installation directories (see the section on Installation Directory
## Variables in the autoconf manual).

edit = sed \
           -e 's,@PERL\@,$(PERL),g' \
           -e 's,@bindir\@,$(bindir),g' \
           -e 's,@STARLINK\@,$(STARLINK),g' \
           -e 's,@SCB_SOURCES\@,$(SCB_SOURCES),g' \
           -e 's,@SCB_INDEX\@,$(SCB_INDEX),g' \
           -e 's,@SCB_BROWSER_TMP\@,$(SCB_BROWSER_TMP),g' \
           -e 's,@SCB_INDEXER_TMP\@,$(SCB_INDEXER_TMP),g' \
           -e 's,@HTX_SERVER\@,$(HTX_SERVER),g'

Scb.pm: Makefile $(srcdir)/Scb.pm.in
	rm -f Scb.pm Scb.pm.tmp
	$(edit) $(srcdir)/Scb.pm.in >Scb.pm.tmp
	mv Scb.pm.tmp Scb.pm

YyTag.pm: Makefile $(srcdir)/YyTag.pm.in
	rm -f YyTag.pm YyTag.pm.tmp
	$(edit) $(srcdir)/YyTag.pm.in >YyTag.pm.tmp
	mv YyTag.pm.tmp YyTag.pm

scb.csh: Makefile $(srcdir)/scb.csh.in
	rm -f scb.csh scb.csh.tmp
	$(edit) $(srcdir)/scb.csh.in >scb.csh.tmp
	mv scb.csh.tmp scb.csh

scb.pl: Makefile $(srcdir)/scb.pl.in
	rm -f scb.pl scb.pl.tmp
	$(edit) $(srcdir)/scb.pl.in >scb.pl.tmp
	chmod +x scb.pl.tmp
	mv scb.pl.tmp scb.pl

scbindex.pl: Makefile $(srcdir)/scbindex.pl.in
	rm -f scbindex.pl scbindex.pl.tmp
	$(edit) $(srcdir)/scbindex.pl.in >scbindex.pl.tmp
	chmod +x scbindex.pl.tmp
	mv scbindex.pl.tmp scbindex.pl

scbcp.pl: Makefile $(srcdir)/scbcp.pl.in
	rm -f scbcp.pl scbcp.pl.tmp
	$(edit) $(srcdir)/scbcp.pl.in >scbcp.pl.tmp
	chmod +x scbcp.pl.tmp
	mv scbcp.pl.tmp scbcp.pl

dbmcat.pl: Makefile $(srcdir)/dbmcat.pl.in
	rm -f dbmcat.pl dbmcat.pl.tmp
	$(edit) $(srcdir)/dbmcat.pl.in >dbmcat.pl.tmp
	chmod +x dbmcat.pl.tmp
	mv dbmcat.pl.tmp dbmcat.pl


# The purpose of this is purely to get Automake to include the .l.c 
# suffix rule in the makefile.  If you can find a less weird way to do it,
# be my guest.
noinst_LIBRARIES = libdummy.a
libdummy_a_SOURCES = dummy-l.l


CLEANFILES = dbmcat.pl Scb.pm YyTag.pm scb.csh scb.pl scbindex.pl scbcp.pl

dist_starnews_DATA = scb.news


