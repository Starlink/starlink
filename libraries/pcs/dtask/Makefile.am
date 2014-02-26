## Process this file with automake to produce Makefile.in

dist_bin_SCRIPTS = alink ilink dtask_link_adam dtask_libtool

lib_LTLIBRARIES = libdtask_adam.la

libdtask_adam_la_SOURCES = \
	$(PUBLIC_INCLUDES) \
	$(include_MESSAGES) \
	$(F_ROUTINES) \
	$(FPP_ROUTINES) \
	$(C_ROUTINES)
# We use ../subpar/SUBPAR_SYS
AM_FCFLAGS = -I../subpar -I../messys -I../adam

include_HEADERS = $(PUBLIC_INCLUDES)
include_MESSAGES = ACT_ERR act_err.h DTASK_ERR dtask_err.h

dist_staretc_DATA = dtask_main.txt

PUBLIC_INCLUDES = DTASK_PAR

PRIVATE_INCLUDES = DTASK_CMN DTASK_SYS

F_ROUTINES = dts_actshut.f dts_act_sched.f dts_addlst.f dts_asthdlr.f	\
	dts_astint.f dts_cancel.f dts_cantim.f dts_comshut.f		\
	dts_dcltask.f dts_dtask.f dts_dumpast.f dts_esetk.f dts_get.f	\
	dts_getpath.f dts_gsoc.f dts_gtcmd.f dts_init.f dts_kick.f	\
	dts_obey.f dts_obeydcl.f dts_prcnam.f dts_remlst.f		\
	dts_resched.f dts_set.f dts_setdump.f dts_srchkey.f		\
	dts_srchlst.f dts_subsid.f dts_timeout.f

FPP_ROUTINES = \
	dts_control.F

C_ROUTINES = \
	dts_chdlr.c \
	dts_setsig.c

CLEANFILES = alink ilink dtask_libtool

EXTRA_DIST = alink.in ilink.in $(PRIVATE_INCLUDES) dtask_err.msg \
             act_err.msg ailink.in.m4 dtask_main.f

MAINTAINERCLEANFILES =  alink.in ilink.in

BUILT_SOURCES = $(include_MESSAGES)

# The alink and ilink scripts require a dtask_main.o file in $(libdir).
# Build and install this, taking care to include it in the manifest file, too.
dtask_main.o: dtask_main.f
install-exec-local: dtask_main.o
	$(mkdir_p) $(DESTDIR)$(libdir)
	$(INSTALL_PROGRAM) dtask_main.o $(DESTDIR)$(libdir)
	$(MANIFEST) && echo "MANIFEST:$(DESTDIR)$(libdir)/dtask_main.o" || :

# Files alink.in and ilink.in depend on the values of the $(prefix)
# variable, and so must be substituted at `make' time, rather than
# `configure' time (see section `Installation Directory Variables' in
# the autoconf manual).  The other substitutions could be done at configure
# time, but it would be pointlessly confusing to do the substititions in
# two phases.  Script dtask_link_adam calls (uninstalled)
# ../subpar/subpar_link adam internally, so we have to add that to
# the PATH.
edit = sed \
    -e 's,@staretcdir\@,$(staretcdir),g' \
    -e 's,@bindir\@,$(bindir),g' \
    -e 's,@libdir\@,$(libdir),g' \
    -e 's,@includedir\@,$(includedir),g' \
    -e 's,@CC\@,$(CC),g' \
    -e 's,@CFLAGS\@,$(CFLAGS),g' \
    -e 's,@FC\@,$(FC),g' \
    -e 's,@FCFLAGS\@,@FCFLAGS@,g' \
    -e 's,@STAR_FCFLAGS\@,@STAR_FCFLAGS@,g' \
    -e 's,@STAR_CPPFLAGS\@,@STAR_CPPFLAGS@,g' \
    -e 's,@STAR_LDFLAGS\@,@STAR_LDFLAGS@,g' \
    -e 's,@FCLIBS\@,$(FCLIBS),g' \
    -e 's,@FCFLAGS\@,@FCFLAGS@,g' \
    -e "s!@DTASK_LINK_ADAM\@!`PATH=../subpar:$$PATH ./dtask_link_adam`!g" \
    -e 's,@PACKAGE_NAME\@,$(PACKAGE_NAME),g' \
    -e 's,@PACKAGE_VERSION\@,$(PACKAGE_VERSION),g' \
    -e '/^\#\#/d'

alink: Makefile $(srcdir)/alink.in
	rm -f alink alink.tmp
	$(edit) \
	    -e 's,@edited_input\@,alink: Generated from alink.in by Makefile.am,' \
	    $(srcdir)/alink.in >alink.tmp
	chmod +x alink.tmp
	mv alink.tmp alink
ilink: Makefile $(srcdir)/ilink.in
	rm -f ilink ilink.tmp
	$(edit) \
	    -e 's,@edited_input\@,ilink: Generated from ilink.in by Makefile.am,' \
	    $(srcdir)/ilink.in >ilink.tmp
	chmod +x ilink.tmp
	mv ilink.tmp ilink

$(srcdir)/alink.in: $(srcdir)/ailink.in.m4
	rm -f alink.in alink.in.tmp
	$(M4) -D__SCRIPTNAME=alink \
	    -D__GENMSG="alink.in: Generated from ailink.in.m4 by Makefile.am" \
	    $(srcdir)/ailink.in.m4 >alink.in.tmp \
	  && mv alink.in.tmp alink.in
$(srcdir)/ilink.in: $(srcdir)/ailink.in.m4
	rm -f ilink.in ilink.in.tmp
	$(M4) -D__SCRIPTNAME=ilink \
	    -D__GENMSG="ilink.in: Generated from ailink.in.m4 by Makefile.am" \
	    $(srcdir)/ailink.in.m4 >ilink.in.tmp \
	  && mv ilink.in.tmp ilink.in
# The installed dtask_libtool is just the libtool we configured for our.
# own use -- it's specific to this configuration, not this directory.
# Note that libtool is located in $(top_builddir) (not part of the
# automake interface, but unlikely to change)
dtask_libtool: $(top_builddir)/libtool
	rm -f dtask_libtool
	$(LN_S) $(top_builddir)/libtool dtask_libtool
