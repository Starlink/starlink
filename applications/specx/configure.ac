dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(specx, 6.8-5, t.jenness@jach.hawaii.edu)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require Starlink automake at least
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([specx.news])

dnl    Include defaults for Starlink configurations
dnl    SPECX is currently per-package-dir but that is not really
dnl    necessary since SYS_SPECX just needs to be in one place.
STAR_DEFAULTS

dnl    Find required versions of the programs we need for configuration
AC_PROG_CC
AC_PROG_FC
AC_PROG_FPP
AC_PROG_LIBTOOL
AC_FC_LIBRARY_LDFLAGS
AC_FC_CHECK_INTRINSICS([ SH SYSTEM ])
AC_FC_RECL_UNIT
AC_FC_OPEN_SPECIFIERS(readonly)
STAR_CHECK_PROGS(hlib)

dnl    In some cases readline requires stdio.h to be pre-loaded
AC_CHECK_HEADERS([readline/readline.h],,,[#include <stdio.h>])
AC_CHECK_HEADERS([readline/history.h],,,[#include <stdio.h>])
AC_CHECK_FUNCS([rindex index strchr strrchr])

STAR_CNF_COMPATIBLE_SYMBOLS

dnl    Note that we readline currently depends on termcap
dnl    so to build with readline we need to find something in termcap
dnl    Do not know what happens if this all changes
AC_CHECK_LIB([termcap],[tgetnum])

dnl    Need to find readline library
dnl    Could do with a --with-readline option
AC_CHECK_LIB([readline],[readline])
AC_CHECK_LIB([history],[add_history])
AC_CHECK_FUNCS([readline add_history])


dnl    Build dependencies
STAR_DECLARE_DEPENDENCIES([build], [shl gsd pgplot fio psx chr ndf figaro])

dnl    Documentation
STAR_LATEX_DOCUMENTATION( sun17/ sc8/ )

dnl  But we also need to define the docs subdirs
DOCS_SUBDIRS="hlp sun17 sc8"
AC_SUBST(DOCS_SUBDIRS)


dnl    Need to build subdir makefiles

AC_CONFIG_FILES([Makefile component.xml astro/Makefile command/Makefile exprt/Makefile external/Makefile fitting/Makefile fv4/Makefile gen/Makefile getaline/Makefile graph/Makefile mapdis4/Makefile mv4/Makefile plots/Makefile reduce/Makefile scl/Makefile tek/Makefile term/Makefile util/Makefile vms/Makefile src/Makefile docs/Makefile docs/hlp/Makefile docs/sun17/Makefile docs/sc8/Makefile])

AC_CONFIG_FILES([src/TITLE misc/specx_welcome.txt])
AC_CONFIG_FILES([getaline/getaline_link],[chmod +x getaline/getaline_link])

AC_OUTPUT
