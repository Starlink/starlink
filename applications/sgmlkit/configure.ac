dnl    Process this file with autoconf to produce a configure script

AC_INIT(sgmlkit, 0.7-13, ussc@star.rl.ac.uk)

AC_PREREQ(2.50)
AM_INIT_AUTOMAKE(1.8.2-starlink)

STAR_DEFAULTS

AC_PATH_PROG(PERL, perl)
AC_PATH_PROG(PAX, pax)
AC_PATH_PROGS(JADE, [openjade jade], jade)
AC_PATH_PROGS(SGMLNORM, [osgmlnorm sgmlnorm], sgmlnorm)
AC_PATH_PROG(DVI2BITMAP, dvi2bitmap, dvi2bitmap)
AC_PATH_PROG(PYTHON, python, python)

# The documentation requires Earl Hood's perlSGML library.  See
# http://search.cpan.org/~ehood/perlSGML.1997Sep18/
# Unpack this, and give its location with the --with-perlsgml option
AC_ARG_WITH(perlsgml,
            AS_HELP_STRING([--with-perlsgml],
                           [location of the perlSGML distribution (path to dir w
hich contains lib/SGML/DTD.pm)]),
            PERLSGMLDIR=$withval/lib,
            PERLSGMLDIR=./lib)
# make PERLSGMLDIR absolute
PERLSGMLDIR=`cd $PERLSGMLDIR; pwd`
AC_CHECK_FILE([$PERLSGMLDIR/SGML/DTD.pm],,[PERLSGMLDIR=/UNKNOWN])
AC_SUBST(PERLSGMLDIR)


AC_CONFIG_FILES([Makefile component.xml]
                [doc/dtd2sgml/Makefile doc/ssn70/Makefile]
                [dsssl/sl/common/slparams.dsl])
AC_CONFIG_FILES(lib/img-eqlist.pl, [chmod +x lib/img-eqlist.pl])
# bin scripts are postprocessed from *.x at build time -- see Makefile.am
AC_CONFIG_FILES([bin/sgml2docs.x:bin/sgml2docs.in]
                [bin/sgml2hlp.x:bin/sgml2hlp.in])

AC_OUTPUT
