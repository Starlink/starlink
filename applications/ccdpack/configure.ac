AC_INIT(ccdpack, 4.0-20, ccdpack@starlink.ac.uk)
AC_PREREQ(2.50)
AM_INIT_AUTOMAKE(1.8.2-starlink)
AC_CONFIG_SRCDIR(ccdexercise)
STAR_DEFAULTS(per-package-dirs)
STAR_MONOLITHS
AC_PROG_CC
AC_PROG_FC
AC_PROG_LIBTOOL
STAR_CNF_COMPATIBLE_SYMBOLS

STAR_CHECK_PROGS([hlib ifd2star fgeneric])
AC_FC_LIBRARY_LDFLAGS
AC_FC_MAIN

STAR_MESSGEN(user_err.msg)

TCL_VERSION=8.4
ITCL_VERSION=3.3
IWIDGETS_VERSION=4.0.1

AC_PATH_XTRA

AC_SUBST(TCL_VERSION)
AC_SUBST(ITCL_VERSION)
AC_SUBST(IWIDGETS_VERSION)

LD_LIBRARY_PATH="${STARLINK}/lib:${LD_LIBRARY_PATH}"
export LD_LIBRARY_PATH

# Check availability of Tcl and Tk.  This is declared as a build dependency,
# but we'll get along without it if it's not there because it couldn't
# be built.
#
# Note: we are checking whether a library has been installed amongst
# the Starlink libraries, rather than whether it is present elsewhere
# in the system.  This requires us to temporarily include that
# location in the LDFLAGS prior to the tests below (and the final
# argument of AC_CHECK_LIB is for -l options, not -L options).
# Although the link command which ends up in the Makefile
# automatically includes this extra path, it's not included in the
# link commands used by the AC_CHECK_LIB tests here.  STAR_LDFLAGS is
# not a documented feature of the system, but this whole
# is-it-or-isn't-it Tcl game is getting a bit like a hack already, so
# a little more won't hurt.
my_save_LDFLAGS=$LDFLAGS
LDFLAGS="$STAR_LDFLAGS $LDFLAGS"

AC_CHECK_LIB(tcl$TCL_VERSION,Tcl_Init)
AC_CHECK_LIB(tk$TCL_VERSION,Tk_Init,,,[$X_LIBS $X_EXTRA_LIBS $X_PRE_LIBS -lX11])

dnl   Check for fpu_control.h, this isn't available for all  
dnl   GCC implementations 
AC_CHECK_HEADERS([fpu_control.h])

have_itcl=1
AC_CHECK_LIB(itcl$ITCL_VERSION,Itcl_Init,,[unset have_itcl],[-ltcl$TCL_VERSION])
AC_CHECK_LIB(itk$ITCL_VERSION,Itk_Init,,[unset have_itcl],[-ltk$TCL_VERSION -ltcl$TCL_VERSION])
AC_MSG_CHECKING([whether to use incr tcl])
if test -n "$have_itcl"
then
    AC_MSG_RESULT(yes)
    AC_DEFINE(HAVE_ITCL, 1, [Is incr Tcl present?])
else
    AC_MSG_RESULT(no)
fi
AM_CONDITIONAL(HAVE_ITCL, [test -n "$have_itcl"])

LDFLAGS=$my_save_LDFLAGS

# Note that the starlink Tcl, Tk and ITcl are required, since we need
# some somewhat-private include files.  I'm not sure how respectable
# this is, but it seems unavoidable for what we want to do (for instance,
# itcl uses the same ones).
# Dependencies must be on one line
STAR_DECLARE_DEPENDENCIES([build], [ard ast chr cnf fio generic grp hds idi ifd itcl ndf ndg one par pcs pgplot prm psx tcl tk trn])
STAR_DECLARE_DEPENDENCIES([build], [agi graphpar img pda ref shl sla startcl],[link])
STAR_DECLARE_DEPENDENCIES(configure, [itcl tcl tk])

STAR_LATEX_DOCUMENTATION([sun139])

AC_CONFIG_HEADERS(config.h)
AC_CONFIG_FILES([Makefile component.xml ccdpack.ifd CCD1_PAR
    gen/Makefile  help/Makefile  main/Makefile  tasks/Makefile])
AC_CONFIG_FILES([gen/dgeneric], [chmod +x gen/dgeneric])

AC_OUTPUT
