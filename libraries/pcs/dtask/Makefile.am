## Process this file with automake to produce Makefile.in

bin_SCRIPTS = alink ilink dtask_link_adam dtask_libtool

lib_LTLIBRARIES = libdtask_adam.la

libdtask_adam_la_SOURCES = \
	$(PUBLIC_INCLUDES) \
	$(F_ROUTINES) \
	$(C_ROUTINES)

include_HEADERS = $(PUBLIC_INCLUDES)

PUBLIC_INCLUDES = DTASK_ERR DTASK_PAR dtask_err.h ACT_ERR act_err.h

PRIVATE_INCLUDES = DTASK_CMN DTASK_SYS

F_ROUTINES = \
	dts_actshut.f \
	dts_act_sched.f \
	dts_addlst.f \
	dts_asthdlr.f \
	dts_astint.f \
	dts_cancel.f \
	dts_cantim.f \
	dts_comshut.f \
	dts_control.f \
	dts_dcltask.f \
	dts_dtask.f \
	dts_dumpast.f \
	dts_esetk.f \
	dts_get.f \
	dts_getpath.f \
	dts_gsoc.f \
	dts_gtcmd.f \
	dts_init.f \
	dts_kick.f \
	dts_obey.f \
	dts_obeydcl.f \
	dts_prcnam.f \
	dts_remlst.f \
	dts_resched.f \
	dts_set.f \
	dts_setdump.f \
	dts_srchkey.f \
	dts_srchlst.f \
	dts_subsid.f \
	dts_timeout.f

C_ROUTINES = \
	dts_chdlr.c \
	dts_setsig.c

EXTRA_DIST = alink.in ilink.in

BUILT_SOURCES = ACT_ERR act_err.h DTASK_ERR dtask_err.h

# The alink and ilink scripts require a dtask_main.o file in $(libdir).
# Build and install this, taking care to include it in the MANIFEST_FILE, too.
dtask_main.o: dtask_main.f
install-exec-local: dtask_main.o
	$(INSTALL_PROGRAM) dtask_main.o $(DESTDIR)$(libdir)
	if test -n "$$MANIFEST_FILE"; then \
	    echo "$(DESTDIR)$(libdir)/dtask_main.o" >>$$MANIFEST_FILE; \
	fi

# Files alink.in and ilink.in depend on the values of the $(prefix)
# variable, and so must be substituted at `make' time, rather than
# `configure' time (see section `Installation Directory Variables' in
# the autoconf manual).
edit = sed \
    -e 's,@staretcdir\@,$(staretcdir),g' \
    -e 's,@bindir\@,$(bindir),g' \
    -e 's,@libdir\@,$(libdir),g' \
    -e 's,@CC\@,$(CC),g' \
    -e 's,@CFLAGS\@,$(CFLAGS),g' \
    -e 's,@FC\@,$(FC),g' \
    -e 's,@FCFLAGS\@,$(FCFLAGS),g' \
    -e 's,@FCLIBS\@,$(FCLIBS),g' \
    -e 's,@PACKAGE_NAME\@,$(PACKAGE_NAME),g' \
    -e 's,@PACKAGE_VERSION\@,$(PACKAGE_VERSION),g'

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
	$(M4) -DSCRIPTNAME=alink $(srcdir)/ailink.in.m4 >$@
$(srcdir)/ilink.in: $(srcdir)/ailink.in.m4
	$(M4) -DSCRIPTNAME=ilink $(srcdir)/ailink.in.m4 >$@
# The installed dtask_libtool is just the libtool we configured for our.
# own use -- it's specific to this configuration, not this directory.
dtask_libtool: libtool
	$(LN_S) libtool dtask_libtool
