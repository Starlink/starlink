AC_INIT(ccdpack, 4.0-16, ccdpack@starlink.ac.uk)
AC_PREREQ(2.50)
AM_INIT_AUTOMAKE(1.8.2-starlink)
AC_CONFIG_SRCDIR(ccdexercise)
STAR_DEFAULTS(per-package-dirs)
STAR_MONOLITHS
AC_PROG_FC
AC_PROG_LIBTOOL
STAR_CNF_COMPATIBLE_SYMBOLS
STAR_CHECK_PROGS(atclsh)
STAR_CHECK_PROGS(hlib)
STAR_CHECK_PROGS(ifd2star)
STAR_DECLARE_DEPENDENCIES([build], 
                          [chr agi ard ast chr fio ndf pgplot prm \
                           psx trn pda ndg tclsys startcl])
STAR_LATEX_DOCUMENTATION(sun139)
STAR_LATEX_DOCUMENTATION(ccdpack)
# AC_CONFIG_SUBDIRS([tasks gen help main])
AC_CONFIG_FILES(Makefile component.xml)
AC_OUTPUT
