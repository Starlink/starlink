AC_INIT(ccdpack, 4.0-16, ccdpack@starlink.ac.uk)
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
STAR_FC_LIBRARY_LDFLAGS

STAR_MESSGEN(user_err.msg)

TCL_VERSION=8.2
ITCL_VERSION=3.1
IWIDGETS_VERSION=3.0.0

AC_SUBST(TCL_VERSION)
AC_SUBST(ITCL_VERSION)
AC_SUBST(IWIDGETS_VERSION)

AC_CHECK_LIB(tcl$TCL_VERSION,Tcl_Init)
AC_CHECK_LIB(tk$TCL_VERSION,Tk_Init)
AC_CHECK_LIB(itcl$ITCL_VERSION,Itcl_Init)
AC_CHECK_LIB(itk$ITCL_VERSION,Itk_Init)

# Note that the starlink Tcl, Tk and ITcl are required, since we need
# some somewhat-private include files.  I'm not sure how respectable
# this is, but it seems unavoidable for what we want to do (for instance,
# itcl uses the same ones).
STAR_DECLARE_DEPENDENCIES([build], 
                          [chr agi ard ast chr fio ndf pgplot prm \
                           psx trn pda ndg tcl tk itcl startcl ifd generic])
STAR_LATEX_DOCUMENTATION(sun139)
AC_CONFIG_SUBDIRS([tasks gen help main])
AC_CONFIG_FILES([Makefile component.xml ccdpack.ifd CCD1_PAR])
AC_OUTPUT
