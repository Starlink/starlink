AC_INIT(ccdpack, 4.0-16, ccdpack@starlink.ac.uk)
AC_PREREQ(2.50)
AM_INIT_AUTOMAKE(1.8.2-starlink)
AC_CONFIG_SRCDIR(ccdexercise)
STAR_DEFAULTS(per-package-dirs)
STAR_MONOLITHS
AC_PROG_FC
AC_PROG_LIBTOOL
AC_CHECK_PROG(tclsh)
AC_CHECK_PROG(wish)
STAR_CNF_COMPATIBLE_SYMBOLS
STAR_CHECK_PROGS(atclsh)
STAR_CHECK_PROGS(hlib)
STAR_CHECK_PROGS(ifd2star)
STAR_FC_LIBRARY_LDFLAGS
AC_CHECK_LIB(tcl8.2,Tcl_Init)
AC_CHECK_LIB(tk8.2,Tk_Init)
AC_CHECK_LIB(itcl3.1,Itcl_Init)
AC_CHECK_LIB(itk3.1,Itk_Init)

# Note that the starlink Tcl, Tk and ITcl are required, since we need
# some somewhat-private include files.  I'm not sure how naughty 
# this is.
STAR_DECLARE_DEPENDENCIES([build], 
                          [chr agi ard ast chr fio ndf pgplot prm \
                           psx trn pda ndg tcl tk itcl startcl ifd])
# STAR_LATEX_DOCUMENTATION(sun139)
# STAR_LATEX_DOCUMENTATION(ccdpack)
AC_CONFIG_SUBDIRS([tasks gen help main])
AC_CONFIG_FILES([Makefile component.xml ccdpack.ifd CCD1_PAR])
AC_OUTPUT
