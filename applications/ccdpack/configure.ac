AC_INIT(ccdpack, 4.0-17, ccdpack@starlink.ac.uk)
AC_PREREQ(2.50)
AM_INIT_AUTOMAKE(1.8.2-starlink)
AC_CONFIG_SRCDIR(ccdexercise)
STAR_DEFAULTS(per-package-dirs)
STAR_MONOLITHS
AC_PROG_FC
AC_PROG_LIBTOOL
STAR_CNF_COMPATIBLE_SYMBOLS
STAR_CHECK_PROGS(hlib)
STAR_CHECK_PROGS(ifd2star)
AC_FC_LIBRARY_LDFLAGS

STAR_MESSGEN(user_err.msg)

TCL_VERSION=8.2
ITCL_VERSION=3.1
IWIDGETS_VERSION=3.0.0

AC_SUBST(TCL_VERSION)
AC_SUBST(ITCL_VERSION)
AC_SUBST(IWIDGETS_VERSION)

AC_CHECK_LIB(tcl$TCL_VERSION,Tcl_Init)
AC_CHECK_LIB(tk$TCL_VERSION,Tk_Init)

# Check availability of [incr Tcl].  This is declared as a build dependency,
# but we'll get along without it if it's not there because it couldn't
# be built.
have_itcl=1
AC_CHECK_LIB(itcl$ITCL_VERSION,Itcl_Init,,[unset have_itcl])
AC_CHECK_LIB(itk$ITCL_VERSION,Itk_Init,,[unset have_itcl])
AC_MSG_CHECKING([whether to use incr tcl])
if test -n "$have_itcl"
then
    AC_MSG_RESULT(yes)
    AC_DEFINE(HAVE_ITCL, 1, [Is incr Tcl present?])
else
    AC_MSG_RESULT(no)
fi
AM_CONDITIONAL(HAVE_ITCL, [test -n "$have_itcl"])

# Note that the starlink Tcl, Tk and ITcl are required, since we need
# some somewhat-private include files.  I'm not sure how respectable
# this is, but it seems unavoidable for what we want to do (for instance,
# itcl uses the same ones).
# Dependencies must be on one line
STAR_DECLARE_DEPENDENCIES([build],[chr agi ard ast chr fio ndf pgplot prm psx trn pda ndg tcl tk itcl startcl ifd  img generic sla shl ref graphpar])

STAR_LATEX_DOCUMENTATION(sun139)
STAR_DOCS_FILES([sun139geo.eps sun139reg.eps sun139red.eps sun139mos.eps sun139driz.eps sun139outsky.eps sun139outclear.eps sun139outim.eps])

AC_CONFIG_SUBDIRS([tasks gen help main])
AC_CONFIG_HEADERS(config.h)
AC_CONFIG_FILES([Makefile component.xml ccdpack.ifd CCD1_PAR])
AC_OUTPUT
